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
 * Date: 21.04.2010
 * Time: 10:34:13
 */

trait STIndex extends MutableClauseStorage
        with UnifiableClauseRetrieval
        with VariantClauseRetrieval
        with GeneralClauseRetrieval
        with InstanceClauseRetrieval with Logging {

  // this trait can only be mixed in into ClauseStorage base traits
  this: MutableClauseStorage =>


  //private var tree: SubstitutionIndexTree = EmptyTree()

  //  var posTree = tree
  //  var negTree = tree

  private var posTree: SubstitutionIndexTree = EmptyTree()
  private var negTree: SubstitutionIndexTree = EmptyTree()


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
    // index on all literals of the clause
    for (literal <- a.literals) {
      add(literal)
    }

    super.add(a)

  }


  private def add(term: FOLNode) {
    // TODO , maybe save old copies for undo redo functionality

    term match {
      case term if (term.positive) => posTree = posTree.insert(term)
      case Negation(term) => negTree = negTree.insert(term)
    }

    //    term match {
    //      case term if (term.positive) => posTree = posTree.insert(term)
    //      case Negation(term) => negTree = negTree.insert(term)
    //    }

  }

  private def remove(term: FOLNode) {
    //throw new NotImplementedException
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



  private def retrieveUnifiableLeafNodes(queryTerm: FOLNode): List[SubstitutionIndexTree] = {
    queryTerm match {
      case term if (term.positive) => {
        // create the query substitution
        val queryS = Substitution(Map(Variable("queryV") -> term))
        // get the nodes from the tree
        val nodes = posTree.retrieveUnifiable(queryS)

        // extract only leaf nodes
        val leafNodes = nodes.filter(_ match {
          case LeafNode(_, _,_) => true
          case _ => false
        }).asInstanceOf[List[LeafNode]]
        // extract the stored terms
        leafNodes
      }
      case Negation(term) => {
        // create the query substitution
        val queryS = Substitution(Map(Variable("queryV") -> term))
        // get the nodes from the tree
        val nodes = negTree.retrieveUnifiable(queryS)
        // extract only leaf nodes
        val leafNodes = nodes.filter(_ match {
          case LeafNode(_, _,_) => true
          case _ => false
        }).asInstanceOf[List[LeafNode]]
        // extract the stored terms
        leafNodes

      }

    }

  }


  private def retrieveUnifiableTerms(queryTerm: FOLNode): List[FOLNode] = {
    val terms = queryTerm match {
      case term if (term.positive) => {
        // create the query substitution
        val queryS = Substitution(Map(Variable("queryV") -> term))
        // get the nodes from the tree
        val nodes = posTree.retrieveUnifiable(queryS)
        // extract only leaf nodes
        val leafNodes = nodes.filter(_ match {
          case LeafNode(_, _,_) => true
          case _ => false
        }).asInstanceOf[List[LeafNode]]
        // extract the stored terms
        leafNodes.flatMap(_.terms)
      }
      case Negation(term) => {
        // create the query substitution
        val queryS = Substitution(Map(Variable("queryV") -> term))
        // get the nodes from the tree
        val nodes = negTree.retrieveUnifiable(queryS)
        // extract only leaf nodes
        val leafNodes = nodes.filter(_ match {
          case LeafNode(_, _,_) => true
          case _ => false
        }).asInstanceOf[List[LeafNode]]
        // extract the stored terms
        leafNodes.flatMap(_.terms).map(_.negate)

      }

    }

    terms

  }


  private def retrieveVariantTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException

  }

  private def retrieveInstanceTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException
  }

  private def retrieveGeneralTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException
  }


}

trait STHeadIndex extends MutableClauseStore
        with ForwardMatchingGeneralClauseRetrieval {

  // this trait can only be mixed in into ClauseStorage base traits
  this: MutableClauseStorage =>


  // only index the clause heads
  private var tree: SubstitutionIndexTree = EmptyTree()

  //  val termToClause: scala.collection.mutable.MultiMap[FOLNode, FOLClause] =
  //  new HashMap[FOLNode, MSet[FOLClause]] with MultiMap[FOLNode, FOLClause]


  abstract override def removeNext: FOLClause = {
    // remove from headindex
    val clause = super.removeNext
    remove(clause.literals.toList.first)
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
    // index the head of the clause
    add(a.literals.toList.head)

    super.add(a)


  }


  private def add(term: FOLNode) {
    // TODO , maybe save old copies for undo redo functionality
    // add into index
    tree = tree.insert(term)
  }

  private def remove(term: FOLNode) {
    throw new NotImplementedException
  }



  // this will search for generalizations only in the heads of the indexed clauses
  override def retrieveForwardMatchingGeneralizations(queryTerm: FOLNode) = {
    // first get the terms from the head index
    val terms = retrieveGeneralTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause(_))
    clauses

  }




  ///////////////// HERE COMES THE ACTUAL TREE ACCESS /////////////////////////////



  private def retrieveUnifiableTerms(queryTerm: FOLNode): List[FOLNode] = {
    // create the query substitution
    val queryS = Substitution(Map(Variable("queryV") -> queryTerm))

    // get the nodes from the tree
    val nodes = tree.retrieveUnifiable(queryS)

    // extract only leaf nodes
    val leafNodes = nodes.filter(_ match {
      case LeafNode(_, _,_) => true
      case _ => false
    }).asInstanceOf[List[LeafNode]]

    // extract the stored terms
    val terms = leafNodes.flatMap(_.terms)

    terms

  }

  private def retrieveVariantTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException

  }

  private def retrieveInstanceTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException
  }

  private def retrieveGeneralTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException
  }


}


trait SForrestIndex extends MutableClauseStorage
        with UnifiableClauseRetrieval
        with VariantClauseRetrieval
        with GeneralClauseRetrieval
        with InstanceClauseRetrieval with Logging {

  // this trait can only be mixed in into ClauseStorage base traits
  this: MutableClauseStorage =>


  //private var tree: SubstitutionIndexTree = EmptyTree()

  //  var posTree = tree
  //  var negTree = tree

  private var forrest: Map[String, SubstitutionIndexTree] = Map[String, SubstitutionIndexTree]()


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


  private def add(term: FOLNode) {
    // TODO , maybe save old copies for undo redo functionality

    term match {
      case Negation(filler) => {
        val key = "-" + filler.top
        val tree = forrest.get(key) match {
          case Some(tree) => tree.insert(filler)
          case None => EmptyTree().insert(filler)
        }
        // update the map
        forrest += (key -> tree)
      }
      case positiveTerm => {
        val key = positiveTerm.top
        val tree = forrest.get(key) match {
          case Some(tree) => tree.insert(positiveTerm)
          case None => EmptyTree().insert(positiveTerm)
        }

        // update the map
        forrest += (key -> tree)

      }
    }


  }

  private def remove(term: FOLNode) {
    //throw new NotImplementedException
  }


  override def retrieveGeneralizations(queryTerm: FOLNode) = {
    // first get the terms from sti index
    val terms = retrieveGeneralTerms(queryTerm)
    // lookup the clauses for those terms using the term2clause lookup map in the
    val clauses = terms.flatMap(termToClause(_))


    //    val storage = this.toList
    //    // assert here that all retreived clauses do really exist in the core storeage buffer
    //    val containsAll = clauses.forall(storage.contains _ )
    //
    //    if(!containsAll){
    //      log.error("We have retrieved clauses from index that are not contained in the buffer")
    //    }

    clauses

  }


  override def retrieveUnifiables(queryTerm: FOLNode) = {
    // first get the terms from sti index
    val terms = retrieveUnifiableTerms(queryTerm)
    // lookup the clauses for those terms
    val clauses = terms.flatMap(termToClause(_)).removeDuplicates

    // assert here that all retreived clauses do really exist in the core storeage buffer
    //    val containsAll = clauses.forall(this.toList.contains _ )
    //
    //    if(!containsAll){
    //      log.error("We have retrieved clauses from index that are not contained in the buffer")
    //    }

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





  private def retrieveUnifiableTerms(queryTerm: FOLNode): List[FOLNode] = {
    val terms = queryTerm match {
      case Negation(term) => {
        // check if we have a tree
        val key = "-" + term.top
        forrest.get(key) match {
          case Some(tree) => {
            // create the query substitution
            val queryS = Substitution(Map(Variable("queryV") -> term))
            // get the nodes from the tree
            val nodes = tree.retrieveUnifiable(queryS)
            // extract only leaf nodes
            val leafNodes = nodes.filter(_ match {
              case LeafNode(_, _,_) => true
              case _ => false
            }).asInstanceOf[List[LeafNode]]
            // extract the stored terms
            leafNodes.flatMap(_.terms).map(_.negate)
          }
          case None => Nil
        }
      }
      case term => {
        // check if we have a tree
        val key = term.top
        forrest.get(key) match {
          case Some(tree) => {
            // create the query substitution
            val queryS = Substitution(Map(Variable("queryV") -> term))
            // get the nodes from the tree
            val nodes = tree.retrieveUnifiable(queryS)
            // extract only leaf nodes
            val leafNodes = nodes.filter(_ match {
              case LeafNode(_, _,_) => true
              case _ => false
            }).asInstanceOf[List[LeafNode]]
            // extract the stored terms
            leafNodes.flatMap(_.terms)
          }
          case None => Nil
        }
      }
    }
    terms
  }


  private def retrieveVariantTerms(queryTerm: FOLNode): List[FOLNode] = {
    throw new NotImplementedException

  }

  private def retrieveInstanceTerms(queryTerm: FOLNode): List[FOLNode] = {
    val terms = queryTerm match {
      case Negation(term) => {
        // check if we have a tree
        val key = "-" + term.top
        forrest.get(key) match {
          case Some(tree) => {
            // create the query substitution
            val queryS = Substitution(Map(Variable("queryV") -> term))
            // get the nodes from the tree
            val nodes = tree.retrieveInstances(queryS)
            // extract only leaf nodes
            val leafNodes = nodes.filter(_ match {
              case LeafNode(_, _,_) => true
              case _ => false
            }).asInstanceOf[List[LeafNode]]
            // extract the stored terms
            leafNodes.flatMap(_.terms).map(_.negate)
          }
          case None => Nil
        }
      }
      case term => {
        // check if we have a tree
        val key = term.top
        forrest.get(key) match {
          case Some(tree) => {
            // create the query substitution
            val queryS = Substitution(Map(Variable("queryV") -> term))
            // get the nodes from the tree
            val nodes = tree.retrieveInstances(queryS)
            // extract only leaf nodes
            val leafNodes = nodes.filter(_ match {
              case LeafNode(_, _,_) => true
              case _ => false
            }).asInstanceOf[List[LeafNode]]
            // extract the stored terms
            leafNodes.flatMap(_.terms)
          }
          case None => Nil
        }
      }
    }
    terms
  }

  private def retrieveGeneralTerms(queryTerm: FOLNode): List[FOLNode] = {
    val terms = queryTerm match {
      case Negation(term) => {
        // check if we have a tree
        val key = "-" + term.top
        forrest.get(key) match {
          case Some(tree) => {
            // create the query substitution
            val queryS = Substitution(Map(Variable("queryV") -> term))
            // get the nodes from the tree
            val nodes = tree.retrieveGeneral(queryS)
            // extract only leaf nodes
            val leafNodes = nodes.filter(_ match {
              case LeafNode(_, _,_) => true
              case _ => false
            }).asInstanceOf[List[LeafNode]]
            // extract the stored terms
            leafNodes.flatMap(_.terms).map(_.negate)
          }
          case None => Nil
        }
      }
      case term => {
        // check if we have a tree
        val key = term.top
        forrest.get(key) match {
          case Some(tree) => {
            // create the query substitution
            val queryS = Substitution(Map(Variable("queryV") -> term))
            // get the nodes from the tree
            val nodes = tree.retrieveGeneral(queryS)
            // extract only leaf nodes
            val leafNodes = nodes.filter(_ match {
              case LeafNode(_, _,_) => true
              case _ => false
            }).asInstanceOf[List[LeafNode]]
            // extract the stored terms
            leafNodes.flatMap(_.terms)
          }
          case None => Nil
        }
      }
    }
    terms
  }


}


//trait SForrestHeadIndex extends MutableClauseStorage
//         with ForwardMatchingGeneralClauseRetrieval
//        with Logging {
//
//  // this trait can only be mixed in into ClauseStorage base traits
//  this: MutableClauseStorage =>
//
//
//  //private var tree: SubstitutionIndexTree = EmptyTree()
//
//  //  var posTree = tree
//  //  var negTree = tree
//
//  private var forrest: Map[String, SubstitutionIndexTree] = Map[String, SubstitutionIndexTree]()
//
//
//  abstract override def removeNext: FOLClause = {
//    val clause = super.removeNext
//
//    for (term <- clause.literals) {
//      remove(term)
//    }
//
//    clause
//
//
//  }
//
//
//  abstract override def add(a: FOLClause): Unit = {
//    // index the head of the clause
//    add(a.literals.head)
//
//    super.add(a)
//
//  }
//
//  private def add(term: FOLNode) {
//    // TODO , maybe save old copies for undo redo functionality
//
//    term match {
//      case Negation(filler) => {
//        val key = "-" + filler.top
//        val tree = forrest.get(key) match {
//          case Some(tree) => tree.insert(filler)
//          case None => EmptyTree().insert(filler)
//        }
//        // update the map
//        forrest += (key -> tree)
//      }
//      case positiveTerm => {
//        val key = positiveTerm.top
//        val tree = forrest.get(key) match {
//          case Some(tree) => tree.insert(positiveTerm)
//          case None => EmptyTree().insert(positiveTerm)
//        }
//
//        // update the map
//        forrest += (key -> tree)
//
//      }
//    }
//
//
//
//
//  }
//
//  private def remove(term: FOLNode) {
//    //throw new NotImplementedException
//  }
//
//
//  override def retrieveGeneralizations(queryTerm: FOLNode) = {
//    // first get the terms from sti index
//    val terms = retrieveGeneralTerms(queryTerm)
//    // lookup the clauses for those terms using the term2clause lookup map in the
//    val clauses = terms.flatMap(termToClause(_))
//    clauses
//
//  }
//
//
//  override def retrieveUnifiables(queryTerm: FOLNode) = {
//    // first get the terms from sti index
//    val terms = retrieveUnifiableTerms(queryTerm)
//    // lookup the clauses for those terms
//    val clauses = terms.flatMap(termToClause(_)).removeDuplicates
//    clauses
//  }
//
//  override def retrieveUnifiablesFull(queryTerm: FOLNode) = {
//    // first get the terms from sti index
//    val terms = retrieveUnifiableTerms(queryTerm)
//    // lookup the clauses for those terms
//    val clauses = terms.flatMap(termToClause(_)).removeDuplicates
//    // get the subsititutions
//    //val substitutions = retrieveUnifiableLeafNodes(queryTerm).map(_.substitution.get)
//
//
//
//
//    val tuples = terms.flatMap({term => termToClause(term).map({clause => (clause, term)})})
//
//    tuples
//
//
//  }
//
//  override def retrieveInstances(queryTerm: FOLNode) = {
//    // first get the terms from sti index
//    val terms = retrieveInstanceTerms(queryTerm)
//    // lookup the clauses for those terms
//    val clauses = terms.flatMap(termToClause(_))
//    clauses
//  }
//
//  override def retrieveVariants(queryTerm: FOLNode) = {
//    // first get the terms from sti index
//    val terms = retrieveVariantTerms(queryTerm)
//    // lookup the clauses for those terms
//    val clauses = terms.flatMap(termToClause(_))
//    clauses
//  }
//
//
//  ///////////////// HERE COMES THE ACTUAL TREE ACCESS /////////////////////////////
//
//
//
//
//
//  private def retrieveUnifiableTerms(queryTerm: FOLNode): List[FOLNode] = {
//    val terms = queryTerm match {
//      case Negation(term) => {
//        // check if we have a tree
//        val key = "-" + term.top
//        forrest.get(key) match {
//          case Some(tree) => {
//            // create the query substitution
//            val queryS = Substitution(Map(Variable("queryV") -> term))
//            // get the nodes from the tree
//            val nodes = tree.retrieveUnifiable(queryS)
//            // extract only leaf nodes
//            val leafNodes = nodes.filter(_ match {
//              case LeafNode(_, _) => true
//              case _ => false
//            }).asInstanceOf[List[LeafNode]]
//            // extract the stored terms
//            leafNodes.flatMap(_.terms).map(_.negate)
//          }
//          case None => Nil
//        }
//      }
//      case term => {
//        // check if we have a tree
//        val key = term.top
//        forrest.get(key) match {
//          case Some(tree) => {
//            // create the query substitution
//            val queryS = Substitution(Map(Variable("queryV") -> term))
//            // get the nodes from the tree
//            val nodes = tree.retrieveUnifiable(queryS)
//            // extract only leaf nodes
//            val leafNodes = nodes.filter(_ match {
//              case LeafNode(_, _) => true
//              case _ => false
//            }).asInstanceOf[List[LeafNode]]
//            // extract the stored terms
//            leafNodes.flatMap(_.terms)
//          }
//          case None => Nil
//        }
//      }
//    }
//    terms
//  }
//
//
//  private def retrieveVariantTerms(queryTerm: FOLNode): List[FOLNode] = {
//    Nil
//
//  }
//
//  private def retrieveInstanceTerms(queryTerm: FOLNode): List[FOLNode] = {
//    Nil
//  }
//
//  private def retrieveGeneralTerms(queryTerm: FOLNode): List[FOLNode] = {
//    Nil
//  }
//
//
//}
