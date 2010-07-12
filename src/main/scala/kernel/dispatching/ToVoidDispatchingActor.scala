package kernel.dispatching

import allocation.ClauseAllocation
import core.containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.FOLClause
import javax.xml.ws.Dispatch
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import collection.mutable._
import collection.mutable.{MultiMap, Set => MSet}
/**
 * User: nowi
 * Date: 09.02.2010
 * Time: 10:35:14
 */

/**
 * Broadcasts clauses to into nirvana
 */
class ToVoidDispatchingActor extends DispatchingActor {

  override def determineDestination(clauses: Iterable[FOLClause], allocation: scala.collection.immutable.Map[String, Any]) = {
    new HashMap[Actor, MSet[FOLClause]] with MultiMap[Actor, FOLClause]

  }

}