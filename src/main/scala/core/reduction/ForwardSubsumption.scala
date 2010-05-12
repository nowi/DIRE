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
  def apply(clauseBuffer: List[FOLNode], backgroundClauses: ClauseStorage)(implicit subsumptionCheck: Subsumption): Boolean
}




object ForwardSubsumer extends ForwardSubsumption with Logging {
  implicit def listofFOLNode2FOLClause(literals: List[FOLNode]): FOLClause = ALCDClause(literals)

  private def subsumes(clause: FOLClause, allClauses: ClauseStorage)(implicit subsumptionCheck: Subsumption) : Boolean = {
    // get all clausees , check if we have a index
    // we need only to query on one literal from the clause because
    // a subsumer clause will need to be more general in all literals anyway so we cannot miss
    // this is a case of imperfect filtering
    val queryLiteral = clause.literals.head

    val candidateClauses = allClauses match {
    // retrieve based on the index type .. TODO make this check outside this tight loop
    // more in the configuratoin or initialization of the robinson proover !
      case indexedClauseStore: ForwardMatchingGeneralClauseRetrieval => {
        indexedClauseStore.retrieveForwardMatchingGeneralizations(queryLiteral)
      }

      case indexedClauseStore: GeneralClauseRetrieval => {
        // next best index is a matching generalziation supporting index
        indexedClauseStore.retrieveGeneralizations(queryLiteral)
      }

      case _ => {
        // no index support
        log.ifDebug("Performing forward matching without index support !")
        // copy over all clauses from the claus store -- expensive
        allClauses.toList
      }
    }

    // from those candidates try to find one canddate that subsumes the clause
    candidateClauses.find(subsumptionCheck(_, clause)) match {
      case Some(subsumerClause) => {
//        log.debug("%s detected that clause %s is subsumed by %s", this, clause,subsumerClause)
        true
      }

      case None => {
        false
      }
    }
  }


  override def apply(clauseBuffer: List[FOLNode], backgroundClauses: ClauseStorage)(implicit subsumptionCheck: Subsumption) = {
    subsumes(clauseBuffer,backgroundClauses)
  }

}