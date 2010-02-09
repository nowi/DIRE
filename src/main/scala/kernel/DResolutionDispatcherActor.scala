package kernel



import core.containers.{CNFClauseStore, NonEmptyClauseStore, ClauseStorage}
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
class DResolutionDispatcherActor extends DispatchingActor {

  override def determineDestination(clauses: ClauseStorage, allocation: Map[Set[String], String]) = {
    //log.trace("Reasoner: %s  clauses %s to reasoners : ", this, clauses,reasoners)
    // get the unique resolvable literal of this clause
    val reasoners = ActorRegistry.actorsFor("kernel.DistributedALCReasoner")
    // create dispatching mapping

    // return mapping allClauses --> allReasoenrs
    reasoners.map({reasoner: Actor => Map(reasoner -> CNFClauseStore())}).reduceLeft(_ ++ _)

  }

}