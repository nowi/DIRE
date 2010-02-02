package reasoner


import core.containers.ClauseStorage
import domain.fol.ast.FOLClause

/**
 * User: nowi
 * Date: 22.01.2010
 * Time: 17:47:10
 */

/**
 * Reasoners 's internal events.
 */
sealed trait Event

case class StartSatisfy(bla: String) extends Event
case class StopSatisfy(bla: String) extends Event
case class Entail(clause: FOLClause) extends Event
case class LoadClauses(clauseStorage: ClauseStorage) extends Event


// administrative messages
case class LoadAllocation(allocation: Map[Set[String], String]) extends Event
