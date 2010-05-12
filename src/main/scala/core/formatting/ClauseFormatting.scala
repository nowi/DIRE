package core.formatting


import containers.ClauseStorage
import domain.fol.ast.FOLClause
import recording.{ClauseRecording, InferenceStep, NaiveClauseRecorder}
import core.containers.ClauseStorage

/**
 * User: nowi
 * Date: 15.12.2009
 * Time: 18:17:35
 */

trait ClauseFormatting {
  def formatClauses(clauses: ClauseStorage, inferenceRecorder: ClauseRecording, showParents: Boolean): String = {
    val buffer = new StringBuffer()
    clauses.toList.map({clause: FOLClause => printInferenceStep(clause, inferenceRecorder)})
            .foreach {buffer.append(_)}
    buffer.toString

  }

//  private def printInferenceStep(clause: FOLClause, inferenceRecorder: InferenceRecording): String = {
//    // ouput format like spass
//    def index = (clause: FOLClause) => (inferenceRecorder.indexOf(clause))
//    inferenceRecorder.getParentsOf(clause) match {
//      case Some((parent1, parent2)) => {
//        // derived clause
//        "%s[Res:%s,%s]" format (index(clause), index(parent1), index(parent2))
//      }
//      case None => {
//        // input clause or foreign
//        "%s[Inp]" format (index(clause))
//
//      }
//    }
//
//
//  }

  // TODO , hack until real clauses can be retrieved from infereence recorder
  def printInferenceStep(clause: FOLClause, inferenceRecorder: ClauseRecording): String = {
    // ouput format like spass
    def index = (clause: FOLClause) => (inferenceRecorder.indexOf(clause))
    inferenceRecorder.getParentsOf(clause) match {
      case Some((parent1:String, parent2:String)) => {
        // derived clause
        "%s[Res:%s,%s]" format (index(clause), parent1, parent2)
      }
      case None => {
        // input clause or foreign
        "%s[Inp]" format (index(clause))

      }
    }


  }


}