package de.unima.dire.core.containers


import de.unima.dire.domain.fol.ast.{FOLNode,Sentence}
import de.unima.dire.core.rewriting.CNFRewriting

import collection.IterableProxy
import collection.mutable.ListBuffer


/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * Conjunctive Normal Form (CNF) : a conjunction of clauses, CLAUSe AND CLAUSE AND CLAUSE
 *
 */
case class CNFClauseStore(override val self: List[FOLClause]) extends IterableProxy[FOLClause]
        with ClauseStorage with CNFRewriting {

  override def isEmpty: Boolean = {
    self.isEmpty
  }

  override def size = self.size

  override def toString = "ClauseStore : %s" format (this mkString ("(", "\n", ")"))

}

object CNFClauseStore {
  def apply(): ClauseStorage = CNFClauseStore(List[FOLClause]())


  def apply(params: FOLClause*): ClauseStorage = {
    CNFClauseStore(List(params: _*))
  }


  def apply(set: Set[FOLClause]): ClauseStorage = {
    CNFClauseStore(List() ++ set)
  }

  def apply(iterable: Iterable[FOLClause]): ClauseStorage = {
    CNFClauseStore(List() ++ iterable)
  }


  def apply(clauseStore: ClauseStorage): ClauseStorage = {
    CNFClauseStore(clauseStore)
  }

  //  def apply[B >: FOLClause](seq: Seq[B]):  ClauseStorage[B] = {
  //    CNFClauseStore(seq.toList)
  //  }

}


// extractors






