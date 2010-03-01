package core.formatting


import containers.ClauseStorage
import domain.fol.ast.FOLClause
import resolution.{InferenceStep, InferenceRecording}
import core.containers.ClauseStorage

/**
 * User: nowi
 * Date: 15.12.2009
 * Time: 18:17:35
 */

trait ClauseFormatting {
  def formatClauses(clauses: ClauseStorage, inferenceRecorder: InferenceRecording,showParents : Boolean): String = {
    val buffer = new StringBuffer()

    showParents match {
      case true => clauses.map({clause: FOLClause => printInferenceStep(inferenceRecorder.inferenceSteps(clause),inferenceRecorder)})
            .foreach {buffer.append(_)}
      case false =>  clauses.map({clause: FOLClause => "[%s]\t%s\n" format (inferenceRecorder.indexOf(clause),clause) })
            .foreach {buffer.append(_)}
    }

    buffer.toString

  }


  def printInferenceStep(is: InferenceStep[FOLClause],inferenceRecorder: InferenceRecording): String = {
    is match {
      case InferenceStep(value, Some(left), Some(right)) => {
        val leftIndex = inferenceRecorder.indexOf(left.value)
        val rightIndex = inferenceRecorder.indexOf(right.value)
        val resolvedIndex = inferenceRecorder.indexOf(value)


        "[%s]%s\n\t\t ==> [%s]%s\n[%s]%s\n" format (leftIndex,left.value,resolvedIndex, "\t" + value,rightIndex, right.value)
      }

      case InferenceStep(value, None, None) => {
        val resolvedIndex = inferenceRecorder.indexOf(value)
        "[%s]%s" format (resolvedIndex,value)
      }
    }


  }


}