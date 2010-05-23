package kernel.dispatching


import allocation.ClauseAllocation
import collection.immutable.Map
import collection.mutable._
import collection.mutable.{MultiMap, Set => MSet}
import core.caches.URLitCache
import core.containers.{MutableClauseStorage, CNFClauseStore, ClauseStorage}
import core.resolution.UniqueLiteralResolution
import domain.fol.ast.FOLClause
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.dispatch.Dispatchers

/**
 * User: nowi
 * Date: 09.02.2010
 * Time: 10:36:18
 */

/**
 * Sends clauses accoring to Distributed Resolution Rules
 */
class DALCDispatcherActor(env: {val uniqueLiteralResolver: UniqueLiteralResolution; val uniqueRLitCache: URLitCache})
        extends DispatchingActor {
  override def determineDestination(clauses: Iterable[FOLClause], allocation: scala.collection.immutable.Map[String,String]) = {
    //record.trace("Reasoner: %s  clauses %s to reasoners : ", this, clauses,reasoners)
    // get the unique resolvable literal of this clause

    // TODO add short circuit here if locally responsible ... ?

    // create dispatching mapping

    // create multimap actor --> List(clauses)
    val mapping: scala.collection.mutable.MultiMap[Actor, FOLClause] = new HashMap[Actor, MSet[FOLClause]] with MultiMap[Actor, FOLClause]


    implicit val uniqueLiteralResolver = env.uniqueLiteralResolver

    implicit val uniqueRLitCache = env.uniqueRLitCache

    // check the URLit of each clPOm.xmlause
    for (clause <- clauses) {
      // get the reasoner that is responsible for this clause

      // get the urlit
      clause.uniqueResolvableLit(uniqueLiteralResolver,uniqueRLitCache) match {
        case Some(urLit) => {

          // TODO hack until i get the latest akka buidling
          ActorRegistry.actorFor(allocation(urLit.top)) match {
            case Some(actor) if (actor != parent.get) => {
              mapping.add(actor, clause) // found one actor OK!
            }
            case Some(actor) if (actor == parent.get) => {
              log.error("This clause stays here , should not happen anymore")
            }
            case None => {
              log.error("No actor found for this symbol , cannot dispatch derived clause !")

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

}