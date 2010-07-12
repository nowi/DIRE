package kernel.dispatching


import allocation.ClauseAllocation
import collection.immutable.Map
import collection.mutable._
import collection.mutable.{MultiMap, Set => MSet}
import core.caches.{ActorCache, URLitCache}
import core.containers.{MutableClauseStorage, CNFClauseStore, ClauseStorage}
import core.resolution.UniqueLiteralResolution
import domain.fol.ast.FOLClause
import recording.EventRecording
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.config.ScalaConfig.RemoteAddress
import se.scalablesolutions.akka.dispatch.Dispatchers
import se.scalablesolutions.akka.remote.Cluster

/**
 * User: nowi
 * Date: 09.02.2010
 * Time: 10:36:18
 */

/**
 * Sends clauses accoring to Distributed Resolution Rules
 */
class DALCDispatcherActor(env: {val uniqueLiteralResolver: Option[UniqueLiteralResolution]; val uniqueRLitCache: URLitCache; val eventRecorder: Option[EventRecording]; val actorCache: Option[ActorCache]})
        extends DispatchingActor {
  override def determineDestination(clauses: Iterable[FOLClause], allocation: scala.collection.immutable.Map[String, Any]) = {
    //record.trace("Reasoner: %s  clauses %s to reasoners : ", this, clauses,reasoners)
    // get the unique resolvable literal of this clause

    // TODO add short circuit here if locally responsible ... ?

    // create dispatching mapping

    // create multimap actor --> List(clauses)
    val mapping: scala.collection.mutable.MultiMap[Actor, FOLClause] = new HashMap[Actor, MSet[FOLClause]] with MultiMap[Actor, FOLClause]

    val eventRecorder = env.eventRecorder


    implicit val uniqueLiteralResolver = env.uniqueLiteralResolver

    implicit val uniqueRLitCache = env.uniqueRLitCache

    implicit val actorCache = env.actorCache

    // check the URLit of each clPOm.xmlause
    for (clause <- clauses) {
      // get the reasoner that is responsible for this clause

      // get the urlit
      clause.uniqueResolvableLit(uniqueLiteralResolver, uniqueRLitCache) match {
        case Some(urLit) => {

          // TODO hack until i get the latest akka buidling
          val actors = ActorRegistry.actors
          val actoradress = allocation(urLit.top)
          lookupActor(actoradress) match {
            case Some(actor) if (actor != parent.get) => {

              // record this if there is a recorder
              eventRecorder match {
                case Some(recorder) => {
                  recorder.recordDispatchedClause(clause, actoradress.toString)
                }

                case None => // no inference recorder present
              }



              mapping.add(actor, clause) // found one actor OK!
            }
            case Some(actor) if (actor == parent.get) => {
              log.error("This clause stays here , should not happen anymore")
            }

            case Some(actor) => {
              // found actor -- strange that

            }
            case None => {
              log.error("No actor found for this symbol %s , cannot dispatch derived clause !", urLit.top)

              //error("No actor found for this symbol , cannot dispatch derived clause !")
            }
          }

        }
        case None => {
          // no unique resolvable literal -- we do not send this
          log.info("No unique resolvable literal for clause %s .. we do not dispatch!", clause)

        }
      }


    }

    mapping


  }

  def lookupActor(address: Any): Option[Actor] = {
    address match {
      case endpoint: RemoteAddress => {
        val found: Actor = se.scalablesolutions.akka.remote.RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port)
        Some(found)
      }
      case uuid: String => {
        val found = ActorRegistry.actorFor(uuid)
        found
      }
      case _ => {
        error("Unknown remote address type : %s" format address)
        None
      }
    }
  }


  /**
   * Use this method for lookup uuid --> actor , this can vary depending on the current setting
   * usage of ActorRegistry will only work if all nodes are in the same process space
   *
   * If not , then Clustor lookup has to be done , maybe cache this somewhere
   *
   */
  //  def lookupActorForUUID(uuid: String)(implicit cache: Option[ActorCache]): Option[Actor] = {
  //    cache match {
  //      case Some(cache) => {
  //        ActorRegistry.actorFor(uuid) match {
  //          case Some(actor) => {
  //            // found
  //            // update ca
  //            cache.update(uuid, Some(actor))
  //            Some(actor)
  //
  //          }
  //
  //          case None => {
  //            // not found in actor reg, now check in cluster
  //
  //            //            var found: Option[Actor] = None
  //            //            for (endpoint: RemoteAddress <- se.scalablesolutions.akka.remote.Cluster) {
  //            //              val actor = se.scalablesolutions.akka.remote.RemoteClient.actorFor(uuid, endpoint.hostname, endpoint.port)
  //            //              println(actor.uuid + "doeas not equal " + uuid)
  //            //              if (actor.uuid == uuid) found = Some(actor)
  //            //            }
  //
  //
  //            //            val found: Option[Actor] = se.scalablesolutions.akka.remote.Cluster.lookup({
  //            //              case RemoteAddress(hostname, port) => se.scalablesolutions.akka.remote.RemoteClient.actorFor(uuid, hostname, port)
  //            //            })
  //
  //            val found = se.scalablesolutions.akka.remote.RemoteClient.actorFor(classOf[DefaultDALCReasoner].toString, endpoint.hostname, endpoint.port)
  //
  //            // add this to cache
  //            cache.update(uuid, found)
  //            found
  //          }
  //        }
  //
  //      }
  //      case None => {
  //        // no cache
  //        ActorRegistry.actorFor(uuid) match {
  //          case Some(actor) => {
  //            // found
  //            // update ca
  //
  //            cache match {
  //              case Some(cache) => {
  //                cache.update(uuid, Some(actor))
  //              }
  //              case None => // no cache
  //            }
  //            Some(actor)
  //          }
  //
  //          case None => {
  //            // not found in actor reg, now check in cluster
  //
  //            var found: Option[Actor] = None
  //            for (endpoint: RemoteAddress <- se.scalablesolutions.akka.remote.Cluster) {
  //              val actor = se.scalablesolutions.akka.remote.RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port)
  //              if (actor.uuid == uuid) found = Some(actor)
  //            }
  //
  //            // add this to cache
  //            cache match {
  //              case Some(cache) => cache.update(uuid, found)
  //              case None => // no caching
  //            }
  //            found
  //          }
  //        }
  //
  //
  //      }
  //    }
  //
  //
  //  }


}