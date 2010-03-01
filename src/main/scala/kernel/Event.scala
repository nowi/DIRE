package kernel


import core.containers.ClauseStorage
import core.ProvingResult
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
case class Entail(clauses: ClauseStorage) extends Event
case class Result(result : ProvingResult) extends Event
case class LoadClauses(clauseStorage: ClauseStorage) extends Event
case class GetKeptClauses(bla : String) extends Event
case class KeptClauses(clauses : ClauseStorage) extends Event


// administrative messages
case class LoadAllocation(allocation: Map[Set[String], String]) extends Event
case class ProverStatus(status: ProvingState) extends Event
case class Status(status: String) extends Event
case class GetStatus(bla : String) extends Event


