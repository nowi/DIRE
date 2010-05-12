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
  private var inferenceTrees: Map[FOLClause, InferenceStep[FOLClause]] = Map[FOLClause, InferenceStep[FOLClause]]()


  private var inferenceLog: Map[FOLClause, Tuple2[FOLClause, FOLClause]] = Map[FOLClause, Tuple2[FOLClause, FOLClause]]()

  private val clauseIndex = MMap[FOLClause,ClauseRecord]()


  override protected def record(clause: FOLClause, parent1: FOLClause, parent2: FOLClause,recieved:Boolean) {
    // put into record
    inferenceLog += (clause -> (parent1, parent2))

    val record = ClauseRecord(Some(parent1),Some(parent2),clause,Some(this))

    // index the record on the derived clause
    clauseIndex.put(clause,record)
  }




  override protected def record(clause: FOLClause,recieved:Boolean) {
    val record = ClauseRecord(None,None,clause,None)
    // index the record on the derived clause
    clauseIndex.put(clause,record)
  }


  def getParentsOf(clause: FOLClause) = {
    val parent1ClauseString: String = "?"
    val parent2ClauseString: String = "?"
    Some(Tuple2(parent1ClauseString, parent2ClauseString))
  }

  def inferenceSteps(c: FOLClause): InferenceStep[FOLClause] = {
    // check if there is not already a tree saved for this clause
    inferenceTrees.get(c) match {
      case Some(tree) => tree
      case None => {
        // there is none , create from all inference steps
        val tree = constructInferenceSteps(c)
        // save the tree
        inferenceTrees += (c -> tree)
        tree
      }
    }

  }


  private def constructInferenceSteps(c: FOLClause): InferenceStep[FOLClause] = {
    // first check if this clause has not already a tree constructed
    inferenceTrees.get(c) match {
      case Some(t) => t
      case None => {
        // create a Tree
        // get the inference parents oft his clause if there are
        inferenceLog.get(c) match {
          case Some((a, b)) => {
            // there are parents
            InferenceStep[FOLClause](c, Some(constructInferenceSteps(a)), Some(constructInferenceSteps(b)))
          }
          case None => {
            // this is clause has not been infered
            InferenceStep[FOLClause](c, None, None)
          }

        }

      }

    }


  }


}

case class InferenceStep[+T](value: T, left: Option[InferenceStep[T]], right: Option[InferenceStep[T]]) {
}