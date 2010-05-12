package core.containers


import domain.fol.ast.FOLNode

/**
 * User: nowi
 * Date: 22.04.2010
 * Time: 13:48:30
 */

trait FOLTermIndex {
  def add(term : FOLNode)
  def remove(term : FOLNode)
  def retrieveUnifiableTerms(queryTerm : FOLNode) : List[FOLNode]
  def retrieveVariantTerms(queryTerm : FOLNode) : List[FOLNode]
  def retrieveInstanceTerms(queryTerm : FOLNode) : List[FOLNode]
  def retrieveGeneralTerms(queryTerm : FOLNode) : List[FOLNode]
}