package de.unima.dire.kernel.dispatching


import de.unima.dire.core.caches.{ActorCache, URLitCache}
import de.unima.dire.core.resolution.UniqueLiteralResolution
import de.unima.dire.recording.EventRecording
import de.unima.dire.core.containers.FOLClause


import se.scalablesolutions.akka.config.ScalaConfig.RemoteAddress
import collection.mutable._
import collection.mutable.{MultiMap, Set => MSet}
import se.scalablesolutions.akka.actor.{ActorRef, ActorRegistry, Actor}
import java.lang.String
import collection.immutable.Map
import collection.Iterable

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

  override def determineDestination(clauses: Iterable[FOLClause], allocation: Map[String, ActorRef]) = {
    //record.trace("Reasoner: %s  clauses %s to reasoners : ", this, clauses,reasoners)
    // get the unique resolvable literal of this clause

    // TODO add short circuit here if locally responsible ... ?

    // create dispatching mapping

    // create multimap actor --> List(clauses)
    val mapping: scala.collection.mutable.MultiMap[ActorRef, FOLClause] = new HashMap[ActorRef, MSet[FOLClause]] with MultiMap[ActorRef, FOLClause]

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

          //val actoradress = allocation(urLit.top)

          allocation.get(urLit.top) match {
            case Some(actor) if (actor != parent.get) => {

              // record this if there is a recorder
              eventRecorder match {
                case Some(recorder) => {
                  recorder.recordDispatchedClause(clause, actor.toString)
                }

                case None => // no inference recorder present
              }



              mapping.addBinding(actor, clause) // found one actor OK!
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


  // TODO this should not be nessesary anymore
//  def lookupActor(address: Any): Option[ActorRef] = {
//    address match {
//      case endpoint: RemoteAddress => {
//        val found = se.scalablesolutions.akka.remote.RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port)
//        Some(found)
//      }
//      case uuid: String => {
//        val found = ActorRegistry.actorFor(uuid)
//        found
//      }
//      case _ => {
//        error("Unknown remote address type : %s" format address)
//        None
//      }
//    }
//  }


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