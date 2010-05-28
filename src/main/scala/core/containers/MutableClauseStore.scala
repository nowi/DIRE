package core.containers


import collection.mutable._
import domain.fol.ast.{EmptyClause, FOLNode, FOLClause}
import scala.collection.mutable.{Set => MSet}

/**
 * User: nowi
 * Date: 20.04.2010
 * Time: 19:43:29
 */

abstract class MutableClauseStore extends MutableClauseStorage {

  override def add(a: FOLClause)

  override def removeNext : FOLClause

  override def remove(a : FOLClause) : FOLClause



  def removeAll(clauses: Iterable[FOLClause]) {
    clauses.foreach(remove _)
  }

  override def addAll(i: Iterable[FOLClause]) : Unit = {
    for (clause <- i)
      add(clause)
  }

}
