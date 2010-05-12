package core.containers


import domain.fol.ast.{FOLNode, FOLClause}
import domain.fol.Substitution

/**
 * User: nowi
 * Date: 21.04.2010
 * Time: 12:53:53
 */

sealed trait ClauseRetrieval {
}


trait UnifiableClauseRetrieval extends ClauseRetrieval {
  def retrieveUnifiables(queryTerm: FOLNode): List[FOLClause]
  def retrieveUnifiablesFull(queryTerm: FOLNode): List[Tuple2[FOLClause,FOLNode]]

}

trait VariantClauseRetrieval extends ClauseRetrieval {
  def retrieveVariants(queryTerm: FOLNode): List[FOLClause]

}

trait GeneralClauseRetrieval extends ClauseRetrieval {
  def retrieveGeneralizations(queryTerm: FOLNode): List[FOLClause]


}

trait InstanceClauseRetrieval extends ClauseRetrieval {
  def retrieveInstances(queryTerm: FOLNode): List[FOLClause]

}

trait ForwardMatchingGeneralClauseRetrieval extends ClauseRetrieval {
  def retrieveForwardMatchingGeneralizations(queryTerm: FOLNode): List[FOLClause]

}

trait BackwardMatchingInstanceClauseRetrieval extends ClauseRetrieval {
  def retrieveBackwardMatchingInstances(queryTerm: FOLNode): List[FOLClause]

}

