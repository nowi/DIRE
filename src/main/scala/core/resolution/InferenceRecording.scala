package core.resolution

import containers.{CNFClauseStore}
import domain.fol.ast._

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 18:49:37
 */

trait InferenceRecording {
  var inferenceTrees: Map[FOLClause, InferenceStep[FOLClause]] = Map[FOLClause, InferenceStep[FOLClause]]()


  var inferenceLog: Map[FOLClause, Tuple2[FOLClause, FOLClause]] = Map[FOLClause, Tuple2[FOLClause, FOLClause]]()


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