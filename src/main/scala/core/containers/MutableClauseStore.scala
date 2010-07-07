package de.unima.dire.core.containers


import collection.mutable._
import de.unima.dire.domain.fol.ast.{FOLNode}

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

  def removeAll(clauses: Iterable[FOLClause]) : Unit = {
    clauses.foreach(remove _)
  }

  def addAll(i: Iterable[FOLClause]) : Unit = {
    for (clause <- i)
      add(clause)
  }

}
