package recording


import collection.mutable.{ListBuffer}
import domain.fol.ast.FOLClause

/**
 * User: nowi
 * Date: 27.05.2010
 * Time: 13:35:20
 */

class EventRecorder extends EventRecording {

  val _events : ListBuffer[ReasonerEvent] = new ListBuffer[ReasonerEvent]()


  override def events = _events.toList

  override def recordRecievedClause(clause: FOLClause, sender: String) {
    // create a new reasoner event and add it into the events log
    _events.append(RecievedClause(this.toString,System.currentTimeMillis,clause,sender))
  }




  override def recordDispatchedClause(clause: FOLClause, reciever: String) {
    // create a new reasoner event and add it into the events log
    _events.append(DispatchedClause(this.toString,System.currentTimeMillis,clause,reciever))
  }

  override def recordDerivedClause(clause: FOLClause, parent1: FOLClause, parent2: FOLClause) {
    _events.append(DerivedClause(this.toString,System.currentTimeMillis,clause,parent1,parent2))

  }

  override def recordKeptClause(clause: FOLClause, parent1: FOLClause, parent2: FOLClause) {
    _events.append(KeptClause(this.toString,System.currentTimeMillis,clause,parent1,parent2))

  }

  override def recordReducedClause(clause: FOLClause, byClause: FOLClause, byReducer: String) {
    _events.append(ReducedClause(this.toString,System.currentTimeMillis,clause,byClause,byReducer))

  }

  override def recordInputClause(clause: FOLClause) {
    _events.append(InputClause(this.toString,System.currentTimeMillis,clause))

  }
}