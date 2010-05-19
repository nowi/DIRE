package kernel


import allocation.ClauseAllocation
import core.containers.ClauseStorage
import core.ProvingResult
import core.resolution.{SuccessfullReduction, SuccessfullResolution}
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
case class Saturate(clauses: Iterable[FOLClause]) extends Event
case class Derived(derived : FOLClause,parent1:Option[FOLClause],parent2:Option[FOLClause]) extends Event


case class Result(result : ProvingResult) extends Event
case class GetKeptClauses(bla : String) extends Event
case class GetIncomingClausesLog(bla : String) extends Event
case class KeptClauses(clauses : Iterable[FOLClause]) extends Event


// administrative messages
case class LoadAllocation(allocation: ClauseAllocation) extends Event
case class ProverStatus(status: ProvingState,workedOffCount : Int,derivedCount : Int) extends Event
case class Status(status: String) extends Event

case class GetStatus(bla : String) extends Event
case class GetStatusOverride(bla : String) extends Event

case class RecievedClausesCount(val count : Int) extends Event
case class GetRecievedClausesCount(timestamp : String) extends Event

case class DispatchedClausesCount(val count : Int) extends Event
case class GetDispatchedClausesCount(timestamp : String) extends Event


