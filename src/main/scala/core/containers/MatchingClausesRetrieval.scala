package core.containers


import domain.fol.ast.FOLClause
import domain.fol.ast.FOLNode


/**
 * User: nowi
 * Date: 20.12.2009
 * Time: 15:19:45
 */

trait MatchingClausesRetrieval {
  this: ClauseStorage =>

  def getMatchingClauses(node: FOLNode): Option[Set[FOLClause]]
}

trait UnifyingClausesRetrieval {
  this: ClauseStorage =>

  def getUnifyingClauses(node: FOLNode): Option[Set[FOLClause]]
}

trait GeneralizedClausesRetrieval {
  this: ClauseStorage =>

  def getGeneralizedClauses(node: FOLNode): Option[Set[FOLClause]]
}

trait InstantiatingClausesRetrieval {
  this: ClauseStorage =>

  def getInstantiatingClauses(node: FOLNode): Option[Set[FOLClause]]
}