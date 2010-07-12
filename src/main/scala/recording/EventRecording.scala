package recording


import domain.fol.ast.FOLClause

/**
 * User: nowi
 * Date: 27.05.2010
 * Time: 13:31:57
 */

trait EventRecording {
  def recordInputClause(clause: FOLClause): Unit

  def recordDerivedClause(clause: FOLClause, parent1: FOLClause, parent2: FOLClause): Unit

  def recordKeptClause(clause: FOLClause, parent1: FOLClause, parent2: FOLClause): Unit

  def recordRecievedClause(clause: FOLClause, sender: String): Unit

  def recordDispatchedClause(clause: FOLClause, reciever: String): Unit

  def recordReducedClause(clause: FOLClause, byClause: FOLClause, byReducer: String)

  def events: Iterable[ReasonerEvent]

}


sealed trait ReasonerEvent {
  val nodeId: String
  val timestamp: Long
  val clause: FOLClause
}

case class InputClause(override val nodeId: String,override val timestamp: Long,override val clause: FOLClause) extends ReasonerEvent
case class DerivedClause(override val nodeId: String,override val timestamp: Long,override val clause: FOLClause,val parent1 : FOLClause,val parent2 : FOLClause) extends ReasonerEvent
case class KeptClause(override val nodeId: String,override val timestamp: Long,override val clause: FOLClause,val parent1 : FOLClause,val parent2 : FOLClause) extends ReasonerEvent
case class RecievedClause(override val nodeId: String,override val timestamp: Long,override val clause: FOLClause,val senderId : String) extends ReasonerEvent
case class DispatchedClause(override val nodeId: String,override val timestamp: Long,override val clause: FOLClause,val recieverId : String) extends ReasonerEvent
case class ReducedClause(override val nodeId: String,override val timestamp: Long,override val clause: FOLClause,val byClause : FOLClause,val byReducer : String) extends ReasonerEvent