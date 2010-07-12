package recording

import collection.mutable.ListBuffer
import collection.mutable.{Map => MMap}
import domain.fol.ast._

/**
 * User: nowi
 * Date: 17.03.2010
 * Time: 17:27:37
 */

trait ClauseRecording {
  @serializable
  private[recording] val clausesHistory : ListBuffer[FOLClause] = new ListBuffer[FOLClause]()


  def toList : List[(FOLClause,Option[Tuple2[FOLClause, FOLClause]])]

  def recordRecievedClause(clause: FOLClause, parent1: Option[FOLClause], parent2: Option[FOLClause]) = {
     (clause,parent1,parent2) match {

      case (clause, Some(parent1), Some(parent2)) => {
        // put into record
        record(clause,parent1,parent2,true)
      }
      case (clause, None, None) => {
        record(clause,true)
      }

    }
  }


  def recordClause(clause: FOLClause, parent1: Option[FOLClause], parent2: Option[FOLClause]) = {

    (clause,parent1,parent2) match {

      case (clause, Some(parent1), Some(parent2)) => {
        // put into record
        record(clause,parent1,parent2,false)
      }
      case (clause, None, None) => {
        record(clause,false)
      }

    }

  }

  def indexOf(clause : FOLClause) : Int = {
    if(clausesHistory.contains(clause))
      clausesHistory.indexOf(clause)
    else -1;
  }

  //def getParentsOf(clause : FOLClause) : Option[Tuple2[FOLClause,FOLClause]] 
  def getParentsOf(clause : FOLClause) : Option[Tuple2[String,String]] 



  protected def append(clause : FOLClause) {
      // put into quick buffer
      clausesHistory.append(clause)
  }

  protected def record(clause : FOLClause,recieved : Boolean) {
    append(clause)
  }

  protected def record(clause : FOLClause, parent1 : FOLClause,parent2 : FOLClause,recieved : Boolean){
    append(clause)
  }





}