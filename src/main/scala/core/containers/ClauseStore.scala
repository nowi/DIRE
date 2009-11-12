package core.containers


import domain.fol.ast.{FOLClause, EmptyClause, Sentence, Clause}
import rewriting.CNFRewriting

/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * A clause store is a multiset of clauses
 *
 */
trait ClauseStorage {
  val clauses: Set[FOLClause]
  val isEmpty: Boolean
  val containsEmptyClause: Boolean

  /**Add all the elements provided by an iterator
   *  of the iterable object <code>elems</code> to the ClauseStore.
   *
   * @param elems the iterable object containing the elements to be added
   * @return a new set with the elements added.
   */
  def ++(aStore: ClauseStorage): ClauseStorage

  /**Remove all the elements provided by another ClauseStore
   *  of the iterable object <code>elems</code> from the ClauseStore.
   *
   * @param elems the iterable object containing the elements to be removed
   * @return a new set with the elements removed.
   */
  def --(aStore: ClauseStorage): ClauseStorage

}


abstract class ClauseStore extends ClauseStorage {
  override def toString = "ClauseStore : %s" format (clauses mkString ("(", "\n", ")"))
}

/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * Conjunctive Normal Form (CNF) : a conjunction of clauses, where each
 * clause is a disjunction of literals.
 *
 */
case class CNFClauseStore(clauses: Set[FOLClause]) extends ClauseStore with CNFRewriting {
  override lazy val isEmpty: Boolean = {
    clauses.isEmpty
  }

  override lazy val containsEmptyClause: Boolean =
  (clauses exists ((_ match {
    case EmptyClause() => true
    case _ => false
  })))


  def ++(aStore: ClauseStorage) = {
    CNFClauseStore(clauses ++ aStore.clauses)
  }

  def --(aStore: ClauseStorage) = {
    CNFClauseStore(clauses -- aStore.clauses)
  }
}

object CNFClauseStore {
  def apply(): CNFClauseStore = CNFClauseStore(Set[FOLClause]())

  def apply(params: FOLClause*): CNFClauseStore = {
    CNFClauseStore(Set(params: _*))
  }

}


object EmptyClauseStore {
  def unapply(clauseStore: ClauseStorage): Option[ClauseStorage] = {
    if (clauseStore.isEmpty)
      Some(clauseStore)
    else
      None

  }
}

object ClauseStoreContainingEmptyClause {
  def unapply(clauseStore: ClauseStorage): Option[ClauseStorage] = {
    if (clauseStore.clauses.exists(_.isInstanceOf[EmptyClause]))
      Some(clauseStore)
    else
      None

  }
}

object NonEmptyClauseStore {
  def unapply(clauseStore: ClauseStorage): Option[ClauseStorage] = {
    clauseStore match {
      case ClauseStoreContainingEmptyClause(x) => None
      case _ if (!clauseStore.isEmpty) => Some(clauseStore)
      case _ => None
    }

  }
}


