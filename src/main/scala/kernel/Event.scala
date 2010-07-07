package de.unima.dire.kernel


import de.unima.dire.core.ProvingResult
import de.unima.dire.recording.ReasonerEvent
import de.unima.dire.core.containers.FOLClause
import se.scalablesolutions.akka.actor.ActorRef

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
case class SaturateInitial(clauses: Iterable[FOLClause]) extends Event
case class Derived(derived: FOLClause, parent1: Option[FOLClause], parent2: Option[FOLClause]) extends Event
case class GivenClause(given: FOLClause) extends Event
case class DerivedBatch(derived: Iterable[FOLClause]) extends Event


case class Result(result: ProvingResult) extends Event
case class GetKeptClauses(sessionToken: String) extends Event
case class GetIncomingClausesLog(sessionToken: String) extends Event
case class KeptClauses(clauses: Iterable[FOLClause]) extends Event
case class GetEventLog(sessionToken: String) extends Event
case class EventLog(eventLog: Iterable[ReasonerEvent]) extends Event






// administrative messages
case class LoadAllocation(allocation: Map[String, ActorRef], reasoner: ActorRef) extends Event
case class LocalSymbols(localSymbols: List[String]) extends Event
case class ProverStatus(status: ProvingState, workedOffCount: Int, derivedCount: Int, recievedKeptClauseCount: Int, recievedClauseCount: Int, dispatchedClauseCount: Int) extends Event
case class Status(status: String) extends Event
case class Heartbeat(status: String) extends Event
case class PurgeClauseBuffer extends Event
case class Shutdown(bla: String) extends Event

case class GetStatus(bla: String) extends Event
case class GetStatusOverride(bla: String) extends Event

case class RecievedClausesCount(val count: Int) extends Event
case class GetRecievedClausesCount(timestamp: String) extends Event

case class DispatchedClausesCount(val count: Int) extends Event
case class GetDispatchedClausesCount(timestamp: String) extends Event



case class RegisterParentActor(actor: ActorRef) extends Event



