package core.containers

import collection.mutable.{HashMap, MultiMap}
import domain.fol.ast._
import scala.collection.{Set => ISet}
import scala.collection.mutable.{Set => MSet}


import scala.collection.{Map}

/**
 * User: nowi
 * Date: 16.12.2009
 * Time: 18:32:31
 */
class NaiveFOLClauseIndex extends ImPerfectFilteringFOLClauseIndex {
  // concrete index

  val index: MultiMap[Tuple2[String, Int], FOLClause] =
  new HashMap[Tuple2[String, Int], MSet[FOLClause]] with MultiMap[Tuple2[String, Int], FOLClause]


  def size = (0 /: index.values.map {_.size})(_ + _)


  def isEmpty = (size == 0)

  private def generateKey(node: FOLNode): Tuple2[String, Int] = {
    // generate the signature for this node
    // TODO there should be a guard here checking if the passed In nodes are literals
    // but this does not work currently because of seome bug in the positive negatitbe literal
    // case class extractors.
    val key = node match {
      case Negation(Negation(filler)) => (filler.top, filler.arity)
      case Negation(filler) => ("-" + filler.top, filler.arity)
      case filler: FOLNode => (filler.top, filler.arity)
    }
    key

  }


  // index only on the signature of the literals
  private def generateKeys(clause: FOLClause) = {

    clause.signature
  }



  // do indexing on all literals
  def retrieve(node: FOLNode) = {
    index.get(generateKey(node)) match {
      case Some(set: scala.collection.mutable.Set[FOLClause]) if (!set.isEmpty) => Some(Set[FOLClause]() ++ set)
      case _ => None

    }

  }

  def insert(clause: FOLClause) {
    generateKeys(clause).foreach {
      index.add(_, clause)
    }


  }


  def insertAll(clauses: Iterable[FOLClause]) = {
    for (clause <- clauses) insert(clause)
  }

  def insertAll(clauses: FOLClause*) {
    clauses.foreach({insert(_)})

  }

  def delete(clause: FOLClause) {
    generateKeys(clause).foreach {
      index.remove(_, clause)
    }
  }

}