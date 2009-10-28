package core.containers


import domain.fol.ast.Sentence

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 16:56:07
 *
 * Container class that wraps a clause store collection container , and has tell and ask
 * methods implemented, uses FOLDSL parser for plaintext tell functionality possible
 */

class FOLKnowledgeBase {
  val clauses: ClauseStoreCollection = ClauseStoreCollection(Set())

  def tell(sentence: Sentence) {
    //    clauses = clauses.clauseStores ++ CNFClauseStore(sentence)

  }


}