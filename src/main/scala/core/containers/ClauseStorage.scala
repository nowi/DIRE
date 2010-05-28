package core.containers

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 19:29:15
 */

import collection.mutable.ListBuffer
import collection.mutable.{Map => MMap}
import domain.fol.ast.{FOLNode, FOLClause, Sentence}

trait NewClauseStorage {
  val index : FOLTermIndex

  val containsEmptyClause: Boolean

  def signature: List[String]

}


/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * A clause store is a multiset of clauses
 *
 */
trait ClauseStorage {
  def signature: Iterable[String] = {
    toList.flatMap({_.literals}).map({literal: FOLNode => literal.top})
  }

  def hasNext : Boolean = !isEmpty
  
  def isEmpty : Boolean

  def size : Int

  def toList : List[FOLClause]
}


trait MutableClauseStorage extends ClauseStorage {
  def removeNext: FOLClause
  def remove(a : FOLClause) : FOLClause
  def add(a: FOLClause) : Unit
  def addAll(i: Iterable[FOLClause]) : Unit
//  def addAll(seq: Seq[FOLClause]) : Unit
  def removeAll(clauses : Iterable[FOLClause]) : Unit

  protected val termToClause: scala.collection.mutable.MultiMap[FOLNode, FOLClause]
}

// implicit converstion form clause storage --> List[FOLClause]

object ClauseStorage {
      implicit def listFOLClause2CNFClauseStorage(list : List[FOLClause]) : CNFClauseStore = CNFClauseStore(list)
  implicit def CNFClauseStorage2listFOLClause2(clauseStore : CNFClauseStore) : List[FOLClause] = clauseStore.toList

}
