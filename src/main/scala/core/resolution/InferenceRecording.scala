package core.resolution

import collection.mutable.ListBuffer
import collection.mutable.{Map => MMap}
import containers.{CNFClauseStore}
import domain.fol.ast._

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 18:49:37
 */
case class ClauseRecord(a:Option[FOLClause],b:Option[FOLClause],resolvent:FOLClause,resolver : Option[Resolution]) {

  val timestamp = System.currentTimeMillis
}

trait InferenceRecording {
  self : core.resolution.Resolution =>
  private var inferenceTrees: Map[FOLClause, InferenceStep[FOLClause]] = Map[FOLClause, InferenceStep[FOLClause]]()


  private var inferenceLog: Map[FOLClause, Tuple2[FOLClause, FOLClause]] = Map[FOLClause, Tuple2[FOLClause, FOLClause]]()

  private val clausesHistory : ListBuffer[ClauseRecord] = new ListBuffer[ClauseRecord]()

  private val clauseIndex = MMap[FOLClause,ClauseRecord]()



  def addInferredClause(parent1 : FOLClause,parent2 : FOLClause , resolvent : FOLClause) = {
    // put into log
    inferenceLog += (resolvent -> (parent1, parent2))

    val record = ClauseRecord(Some(parent1),Some(parent2),resolvent,Some(this))

    // create and add inference record , insert into history buffer
    clausesHistory.append(record)

    // index the record on the derived clause
    clauseIndex.put(resolvent,record)



  }

  def addInitialClause(clause: FOLClause) = {
    val record = ClauseRecord(None,None,clause,None)
    // create and add inference record , insert into history buffer
    clausesHistory.append(record)
    // index the record on the derived clause
    clauseIndex.put(clause,record)

  }


  def indexOf(clause : FOLClause) = {
    clausesHistory.indexOf(clauseIndex(clause))
  }

  def recordOf(clause : FOLClause) = {
    clauseIndex(clause)
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