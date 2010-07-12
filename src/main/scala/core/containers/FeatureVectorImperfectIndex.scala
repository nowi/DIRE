package core.containers

import collection.mutable.{HashMap, MultiMap, Set => MSet}
import domain.fol.ast.{Negation, Variable, FOLNode, FOLClause}
import domain.fol.Substitution
import helpers.{Logging, HelperFunctions}
import index._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import HelperFunctions._

/**
 * User: nowi
 * Date: 18.05.2010
 * Time: 15:41:03
 *
 * A simple imperfect filtering hashmap based index
 * This index should perform better on the minimum nested term structure
 * we encoutner during ALC resolution
 *
 */

trait FeatureVectorImperfectIndex extends MutableClauseStorage
        with UnifiableClauseRetrieval
        with VariantClauseRetrieval
        with GeneralClauseRetrieval
        with InstanceClauseRetrieval
        with Logging {

  // this trait can only be mixed in into ClauseStorage base traits
  this: MutableClauseStorage =>


  //private var tree: SubstitutionIndexTree = EmptyTree()

  //  var posTree = tree
  //  var negTree = tree

  val positiveLiterals: scala.collection.mutable.MultiMap[Integer, FOLNode] =
  new HashMap[Integer, MSet[FOLNode]] with MultiMap[Integer, FOLNode]

  val negativeLiterals: scala.collection.mutable.MultiMap[Integer, FOLNode] =
  new HashMap[Integer, MSet[FOLNode]] with MultiMap[Integer, FOLNode]


  val topSymbolIndex: scala.collection.mutable.MultiMap[String, FOLNode] =
  new HashMap[String, MSet[FOLNode]] with MultiMap[String, FOLNode]


  val hashSeed: Int = 17
  val hashMultiplier: Int = 59

  abstract override def removeNext: FOLClause = synchronized {
    val clause = super.removeNext


    for (term <- clause.literals) {
      remove(term)
    }

    clause


  }


  abstract override def remove(clause: FOLClause): FOLClause = synchronized  {
    val removedClause: FOLClause = super.remove(clause)
    // remove from index ...
    for (term <- removedClause.literals) {
      remove(term)
    }
    removedClause
  }


  abstract override def add(a: FOLClause): Unit = synchronized {
    require(!a.isEmpty)
    // index on all literals of the clause
    for (literal <- a.literals) {
      add(literal)
    }

    super.add(a)

  }


  // compute hash based on arity , topsymbol and polarity
  private def computeHashKey(literal: FOLNode): Int = synchronized {
    var hc: Int = hashSeed
    hc = hc * hashMultiplier + literal.top.hashCode
    hc = hc * hashMultiplier + literal.arity.hashCode
    hc
  }

  private def add(term: FOLNode) = synchronized  {
    // TODO , maybe save old copies for undo redo functionality

    topSymbolIndex.add(term.top, term)
    //require(topSymbolIndex.values.exists(_.contains(term)))

    //    term match {
    //      case Negation(filler) => {
    //        val key = computeHashKey(filler)
    //        // add to index
    //        negativeLiterals.add(key, term)
    //        require(negativeLiterals.values.exists(_.contains(term)))
    //      }
    //      case positiveTerm => {
    //        val key = computeHashKey(term)
    //        // add to index
    //        positiveLiterals.add(key, term)
    //        require(positiveLiterals.values.exists(_.contains(term)))
    //
    //      }
    //    }
  }

  // TODO !!!!!!!!!!!!! IMPLEMENT REMOVAL HERE !!!!!!!!!!!!


  private def remove(term: FOLNode) = synchronized {
//    val bucketsizeBefore = topSymbolIndex.get(term.top).get.size
//
//    topSymbolIndex.get(term.top) match {
//      case Some(bucket) =>
//        if (!bucket.contains(term)) {
//          val b = bucket
//          error("Deleting a term , but the term is not indexed in ites bucket")
//
//        }
//    }

    // TODO check this , do not remove because this is a multi
    //topSymbolIndex.remove(term.top, term)


//    val bucketsizeAfter = topSymbolIndex.get(term.top) match {
//      case Some(bucket) => bucket.size
//      case None => 0
//    }

//    if (bucketsizeAfter > bucketsizeBefore) {
//      require(bucketsizeAfter == bucketsizeBefore - 1)
//    }

    //    term match {
    //      case Negation(filler) => {
    //        val key = computeHashKey(filler)
    //        // add to index
    //        negativeLiterals.remove(key, term)
    //      }
    //      case positiveTerm => {
    //        val key = computeHashKey(term)
    //        // add to index
    //        positiveLiterals.remove(key, term)
    //
    //      }
    //    }
  }


  override def retrieveGeneralizations(queryTerm: FOLNode) = synchronized {
    // first get the terms from sti index
    val terms = retrieveGeneralTerms(queryTerm)
    // lookup the clauses for those terms using the term2clause lookup map in the
    val clauses = terms.flatMap(termToClause.getOrElse(_,Nil))

    clauses

  }


  override def retrieveUnifiables(queryTerm: FOLNode) = synchronized {
    // first get the terms from sti index
    val terms = retrieveUnifiableTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause.getOrElse(_,Nil))

    clauses
  }

  override def retrieveUnifiablesFull(queryTerm: FOLNode) = synchronized {
    // first get the terms from sti index
    val terms = retrieveUnifiableTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause.getOrElse(_,Nil))

    // get the subsititutions
    //val substitutions = retrieveUnifiableLeafNodes(queryTerm).map(_.substitution.get)

    // assert here that all retreived clauses do really exist in the core storeage buffer
    //    val containsAll = clauses.forall(this.toList.contains _ )
    //
    //    if(!containsAll){
    //      log.error("We have retrieved clauses from index that are not contained in the buffer")
    //    }


    val tuples = terms.flatMap({term => termToClause(term).map({clause => (clause, term)})})

    tuples


  }

  override def retrieveInstances(queryTerm: FOLNode) = synchronized {
    // first get the terms from sti index
    val terms = retrieveInstanceTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause.getOrElse(_,Nil))
    clauses
  }

  override def retrieveVariants(queryTerm: FOLNode) = synchronized {
    // first get the terms from sti index
    val terms = retrieveVariantTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause.getOrElse(_,Nil))
    clauses
  }


  ///////////////// HERE COMES THE ACTUAL TREE ACCESS /////////////////////////////




  // imperfect retrieval
  private def retrieveUnifiableTerms(queryTerm: FOLNode): List[FOLNode] = synchronized {


    val results = topSymbolIndex.get(queryTerm.top)

    //    val results = queryTerm match {
    //      case Negation(term) => {
    //        // check if we have a tree
    //        val key = computeHashKey(term)
    //        negativeLiterals.get(key)
    //      }
    //      case term => {
    //        // check if we have a tree
    //        // check if we have a tree
    //        val key = computeHashKey(term)
    //        positiveLiterals.get(key)
    //      }
    //    }

    results match {
      case Some(set) => set.toList
      case None => Nil
    }

  }


  private def retrieveVariantTerms(queryTerm: FOLNode): List[FOLNode] = synchronized {
    throw new NotImplementedException

  }

  private def retrieveInstanceTerms(queryTerm: FOLNode): List[FOLNode] = synchronized {
    val terms = topSymbolIndex.get(queryTerm.top)

    //    val terms = queryTerm match {
    //      case Negation(term) => {
    //        // check if we have a tree
    //        val key = computeHashKey(term)
    //        negativeLiterals.get(key)
    //      }
    //      case term => {
    //        // check if we have a tree
    //        // check if we have a tree
    //        val key = computeHashKey(term)
    //        positiveLiterals.get(key)
    //      }
    //    }

    terms match {
      case Some(set) => set.toList
      case None => Nil
    }
  }

  private def retrieveGeneralTerms(queryTerm: FOLNode): List[FOLNode] = synchronized {
    val terms = topSymbolIndex.get(queryTerm.top)

    //    val terms = queryTerm match {
    //      case Negation(term) => {
    //        // check if we have a tree
    //        val key = computeHashKey(term)
    //        negativeLiterals.get(key)
    //      }
    //      case term => {
    //        // check if we have a tree
    //        // check if we have a tree
    //        val key = computeHashKey(term)
    //        positiveLiterals.get(key)
    //      }
    //    }

    terms match {
      case Some(set) => set.toList
      case None => Nil
    }
  }


}