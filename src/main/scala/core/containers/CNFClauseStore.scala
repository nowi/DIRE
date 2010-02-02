package core.containers


import collection.mutable.ListBuffer
import domain.fol.ast.{FOLNode, FOLClause, EmptyClause, Sentence}
import rewriting.CNFRewriting


/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * Conjunctive Normal Form (CNF) : a conjunction of clauses, CLAUSe AND CLAUSE AND CLAUSE
 *
 */
case class CNFClauseStore(c: List[FOLClause])
        extends ClauseStorage with CNFRewriting {
  private val clauses: List[FOLClause] = c

  override def isEmpty: Boolean = {
    clauses.isEmpty
  }


  def apply(t: Int) = {
    clauses.apply(t)

  }

  override def elements = clauses.elements

  override def length = clauses.length


  override def head = clauses.head

  override def tail = CNFClauseStore(clauses.tail)


  override def filterClauses(f: Function1[FOLClause, Boolean]): ClauseStorage = {
    CNFClauseStore(clauses.filter(f))
  }

  override def ::(x: FOLClause) = {
    CNFClauseStore(x :: clauses)
  }


  def :::(prefix: ClauseStorage) = {
    if (isEmpty) prefix
    else {
      val b = new ListBuffer[FOLClause]
      var those = prefix
      while (!those.isEmpty) {
        b += those.head
        those = those.tail
      }
      CNFClauseStore(b.prependToList(clauses))
    }
  }


  override lazy val containsEmptyClause: Boolean =
  (clauses exists ((_ match {
    case EmptyClause() => true
    case _ => false
  })))

  override def toString = "ClauseStore : %s" format (clauses mkString ("(", "\n", ")"))

}

object CNFClauseStore {
  def apply(): ClauseStorage = CNFClauseStore(List[FOLClause]())


  def apply(params: FOLClause*): ClauseStorage = {
    CNFClauseStore(List(params: _*))
  }


  def apply(set: Set[FOLClause]): ClauseStorage = {
    CNFClauseStore(List() ++ set)
  }

  //  def apply[B >: FOLClause](seq: Seq[B]):  ClauseStorage[B] = {
  //    CNFClauseStore(seq.toList)
  //  }

}


// extractors


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
    if (clauseStore.exists(_.isInstanceOf[EmptyClause]))
      Some(clauseStore)
    else
      None

  }
}

object NonEmptyClauseStore {
  def unapply(clauseStore: ClauseStorage): Option[ClauseStorage] = {
    clauseStore match {
      case ClauseStoreContainingEmptyClause(x) => Some(clauseStore)
      case _ if (!clauseStore.isEmpty) => Some(clauseStore)
      case _ => None
    }

  }
}


