package core.reduction


import containers.{GeneralClauseRetrieval, ClauseStorage, ForwardMatchingGeneralClauseRetrieval}
import domain.fol.ast.{ALCDClause, FOLNode, FOLClause}
import domain.fol.Substitution
import helpers.Logging

/**
 * User: nowi
 * Date: 24.04.2010
 * Time: 15:31:29
 */

trait ForwardSubsumption {
  def apply(clauseBuffer: Set[FOLNode], backgroundClauses: ClauseStorage)(implicit subsumptionCheck: Subsumption): Boolean
}




class ForwardSubsumer(env:{val eventRecorder: Option[recording.EventRecorder]}) extends ForwardSubsumption with Logging {

  val eventRecorder = env.eventRecorder
  implicit def listofFOLNode2FOLClause(literals: Set[FOLNode]): FOLClause = ALCDClause(literals)
  private def subsumes(clause: FOLClause, allClauses: ClauseStorage)(implicit subsumptionCheck: Subsumption) : Boolean = {

//    if(clause.toString == "[¬(O1Author(U))V¬(O1Poster(skf135(U)))]") {
//      log.warning("pay attention")
//
//    }

    val candidateClauses : List[FOLClause] = allClauses match {
    // retrieve based on the index type .. TODO make this check outside this tight loop
    // more in the configuratoin or initialization of the robinson proover !
      case indexedClauseStore: ForwardMatchingGeneralClauseRetrieval => {
        // query on all literals of the clause
        clause.literals.toList.flatMap(indexedClauseStore.retrieveForwardMatchingGeneralizations(_))
      }

      case indexedClauseStore: GeneralClauseRetrieval => {
        // next best index is a matching generalziation supporting index
        clause.literals.toList.flatMap(indexedClauseStore.retrieveGeneralizations(_))
      }

      case _ => {
        // no index support
        // copy over all clauses from the claus store -- expensive
        allClauses.toList
      }
    }


    
    // from those candidates try to find one canddate that subsumes the clause
    candidateClauses.find(subsumptionCheck(_, clause)) match {
      case Some(subsumerClause) => {
        // record this event if there is a recorder present in environment
        eventRecorder match {
          case Some(recorder) => {
              recorder.recordReducedClause(clause,subsumerClause,this.toString)
          }
          case None => // no inference recorder present
        }
        log.debug("%s detected that clause %s is subsumed by %s", this, clause,subsumerClause)
        true
      }

      case None => {
        false
      }
    }
  }


  override def apply(clauseBuffer: Set[FOLNode], backgroundClauses: ClauseStorage)(implicit subsumptionCheck: Subsumption) = {
    subsumes(clauseBuffer,backgroundClauses)
  }

}