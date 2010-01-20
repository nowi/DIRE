package core.formatting


import containers.ClauseStorage
import domain.fol.ast.FOLClause
import resolution.{InferenceStep, InferenceRecording}

/**
 * User: nowi
 * Date: 15.12.2009
 * Time: 18:17:35
 */

trait ClauseFormatting {
  def printClauses(clauses: ClauseStorage, inferenceRecorder: InferenceRecording): String = {
    val buffer = new StringBuffer()

    clauses.map({clause: FOLClause => printInferenceStep(inferenceRecorder.inferenceSteps(clause))})
            .foreach {buffer.append(_)}

    buffer.toString

  }


  def printInferenceStep(is: InferenceStep[FOLClause]): String = {
    is match {
      case InferenceStep(value, Some(left), Some(right)) => {
        "%s\n\t\t ==> %s\n%s\n" format (left.value, "\t" + value, right.value)
      }

      case InferenceStep(value, None, None) => {
        "%s" format (value)
      }
    }


  }


}