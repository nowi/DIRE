package kernel

import core.containers.{CNFClauseStore, NonEmptyClauseStore, ClauseStorage}
import domain.fol.ast.FOLClause
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.dispatch.Dispatchers
/**
 * User: nowi
 * Date: 09.02.2010
 * Time: 10:35:14
 */

/**
 * Broadcasts clauses to into nirvana
 */
class ToVoidDispatchingActor extends DispatchingActor {

  override def determineDestination(clauses: ClauseStorage, allocation: Map[Set[String], String]) = {
    val reasonersUUids = allocation.values.toList
    val reasoners = ActorRegistry.actorsFor("kernel.DistributedALCReasoner")
    log.trace("Reasoner: %s Broadcasting clauses %s to reasoners : ", this, clauses,reasoners)
    // broadcast those clasuses to all known resoners , this is
    // return mapping allClauses --> allReasoenrs
    reasoners.map({reasoner: Actor => Map(reasoner -> CNFClauseStore())}).reduceLeft(_ ++ _)

  }

}