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


  val hashSeed: Int = 17
  val hashMultiplier: Int = 59

  abstract override def removeNext: FOLClause = {
    val clause = super.removeNext


    for (term <- clause.literals) {
      remove(term)
    }

    clause


  }


  abstract override def remove(clause: FOLClause): FOLClause = {
    val removedClause: FOLClause = super.remove(clause)
    // remove from index ...
    for (term <- removedClause.literals) {
      remove(term)
    }
    removedClause
  }


  abstract override def add(a: FOLClause): Unit = {
    require(!a.isEmpty)
    // index on all literals of the clause
    for (literal <- a.literals) {
      add(literal)
    }

    super.add(a)

  }


  // compute hash based on arity , topsymbol and polarity
  private def computeHashKey(literal: FOLNode): Int = {
    var hc: Int = hashSeed
    hc = hc * hashMultiplier + literal.top.hashCode
    hc = hc * hashMultiplier + literal.arity.hashCode
    hc
  }

  private def add(term: FOLNode) {
    // TODO , maybe save old copies for undo redo functionality
    term match {
      case Negation(filler) => {
        val key = computeHashKey(filler)
        // add to index
        negativeLiterals.add(key, term)
      }
      case positiveTerm => {
        val key = computeHashKey(term)
        // add to index
        positiveLiterals.add(key, term)

      }
    }
  }

  private def remove(term: FOLNode) {
    term match {
      case Negation(filler) => {
        val key = computeHashKey(filler)
        // add to index
        negativeLiterals.remove(key, term)
      }
      case positiveTerm => {
        val key = computeHashKey(term)
        // add to index
        positiveLiterals.remove(key, term)

      }
    }
  }


  override def retrieveGeneralizations(queryTerm: FOLNode) = {
    // first get the terms from sti index
    val terms = retrieveGeneralTerms(queryTerm)
    // lookup the clauses for those terms using the term2clause lookup map in the
    val clauses = terms.flatMap(termToClause(_))

    clauses

  }


  override def retrieveUnifiables(queryTerm: FOLNode) = {
    // first get the terms from sti index
    val terms = retrieveUnifiableTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause(_)).removeDuplicates

    clauses
  }

  override def retrieveUnifiablesFull(queryTerm: FOLNode) = {
    // first get the terms from sti index
    val terms = retrieveUnifiableTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause(_)).removeDuplicates

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

  override def retrieveInstances(queryTerm: FOLNode) = {
    // first get the terms from sti index
    val terms = retrieveInstanceTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause(_))
    clauses
  }

  override def retrieveVariants(queryTerm: FOLNode) = {
    // first get the terms from sti index
    val terms = retrieveVariantTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause(_))
    clauses
  }


  ///////////////// HERE COMES THE ACTUAL TREE ACCESS /////////////////////////////




  // imperfect retrieval
  private def retrieveUnifiableTerms(queryTerm: FOLNode): List[FOLNode] = {
    val results = queryTerm match {
      case Negation(term) => {
        // check if we have a tree
        val key = computeHashKey(term)
        negativeLiterals.get(key)
      }
      case term => {
        // check if we have a tree
        // check if we have a tree
        val key = computeHashKey(term)
        positiveLiterals.get(key)
      }
    }

    results match {
      case Some(set) => set.toList
      case None => Nil
    }

  }


  private def retrieveVariantTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException

  }

  private def retrieveInstanceTerms(queryTerm: FOLNode): List[FOLNode] = {
    val terms = queryTerm match {
      case Negation(term) => {
        // check if we have a tree
        val key = computeHashKey(term)
        negativeLiterals.get(key)
      }
      case term => {
        // check if we have a tree
        // check if we have a tree
        val key = computeHashKey(term)
        positiveLiterals.get(key)
      }
    }

    terms match {
      case Some(set) => set.toList
      case None => Nil
    }
  }

  private def retrieveGeneralTerms(queryTerm: FOLNode): List[FOLNode] = {
    val terms = queryTerm match {
      case Negation(term) => {
        // check if we have a tree
        val key = computeHashKey(term)
        negativeLiterals.get(key)
      }
      case term => {
        // check if we have a tree
        // check if we have a tree
        val key = computeHashKey(term)
        positiveLiterals.get(key)
      }
    }

    terms match {
      case Some(set) => set.toList
      case None => Nil
    }
  }


}