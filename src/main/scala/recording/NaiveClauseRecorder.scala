package recording

import collection.mutable.ListBuffer
import collection.mutable.{Map => MMap}
import core.resolution.Resolution
import domain.fol.ast._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 18:49:37
 */
case class ClauseRecord(a:Option[FOLClause],b:Option[FOLClause],resolvent:FOLClause,resolver : Option[Any]) {

  val timestamp = System.currentTimeMillis
}

class NaiveClauseRecorder extends ClauseRecording {

  @serializable
  private var inferenceLog: Map[FOLClause, Tuple2[FOLClause, FOLClause]] = Map[FOLClause, Tuple2[FOLClause, FOLClause]]()


  @serializable
  private val clauseIndex = MMap[FOLClause,ClauseRecord]()


  override def toList = {
    clausesHistory.map(clause => (clause,inferenceLog.get(clause))).toList
  }

  override protected def record(clause: FOLClause, parent1: FOLClause, parent2: FOLClause,recieved:Boolean) {
    super.record(clause,recieved)
    // put into record
    inferenceLog += (clause -> (parent1, parent2))

    val record = ClauseRecord(Some(parent1),Some(parent2),clause,Some(this))

    // index the record on the derived clause
    clauseIndex.put(clause,record)
  }




  override protected def record(clause: FOLClause,recieved:Boolean) {
    super.record(clause,recieved)
    val record = ClauseRecord(None,None,clause,None)
    // index the record on the derived clause
    clauseIndex.put(clause,record)
  }


  override def getParentsOf(clause: FOLClause) = {
    // get the clause record from index
    clauseIndex.get(clause) match {
      case Some(ClauseRecord(Some(parent1),Some(parent2),_,_)) => {
        // has 2 parents
        // index of this record
        Some((indexOf(parent1).toString,indexOf(parent2).toString))
      }

      case _ => {
        None
      }
    }


  }


}

case class InferenceStep[+T](value: T, left: Option[InferenceStep[T]], right: Option[InferenceStep[T]]) {
}