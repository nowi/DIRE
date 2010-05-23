package core.formatting


import containers.ClauseStorage
import domain.fol.ast.{FOLNode, FOLClause}
import recording.{ClauseRecording, InferenceStep, NaiveClauseRecorder}
import core.containers.ClauseStorage

/**
 * User: nowi
 * Date: 15.12.2009
 * Time: 18:17:35
 */

trait ClauseFormatting {
  def formatClauses(clauses: ClauseStorage)(implicit inferenceRecorder: Option[ClauseRecording]): String = {
    val buffer = new StringBuffer()
//    val sortedClaueses = clauses.toList.map({clause: FOLClause => (inferenceRecorder.indexOf(clause),clause)}).toList.sort(_._1 < _._1).map(_._2)
//    sortedClaueses.foreach {clause => buffer.append(printInferenceStep(clause)(inferenceRecorder))}

    clauses.toList.map({clause: FOLClause => printInferenceStep(clause)}).foreach(buffer.append(_))
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
  def printInferenceStep(clause: FOLClause)(implicit inferenceRecorder: Option[ClauseRecording]): String = {
    // ouput format should be defined in subclasses like spass
    def formatLiterals(literals : Iterable[FOLNode]) =  literals mkString ("", " ", "")


    inferenceRecorder match {
      case Some(inferenceRecorder) => {
        def index = (clause: FOLClause) => (inferenceRecorder.indexOf(clause))
        inferenceRecorder.getParentsOf(clause) match {
          case Some((parent1: String, parent2: String)) => {
            // derived clause
            "%s[Res:%s,%s] | %s -> %s\n" format (index(clause), parent1, parent2, formatLiterals(clause.negativeLiterals),formatLiterals(clause.positiveLiterals))
          }
          case None => {
            // input clause or foreign
            "%s[Inp] | %s -> %s\n" format (index(clause),formatLiterals(clause.negativeLiterals),formatLiterals(clause.positiveLiterals))

          }
        }

      }

      // inference recorder is present
      case None => clause.toString

    }



  }


}