package core.containers


import domain.fol.ast.{FOLClause, FOLNode}

/**
 * User: nowi
 * Date: 19.12.2009
 * Time: 22:17:24
 */

trait FOLClauseIndex {
  def insert(clause: FOLClause)

  def insertAll(clauses: scala.Iterable[FOLClause])

  def delete(clause: FOLClause)

  def retrieve(node: FOLNode): Option[Set[FOLClause]]

  def isEmpty: Boolean

}

trait PerfectFilteringFOLClauseIndex extends FOLClauseIndex {
}

trait ImPerfectFilteringFOLClauseIndex extends FOLClauseIndex {
}