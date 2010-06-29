package core.index

import collection.mutable.Stack
import domain.fol.ast._
import domain.fol.functions.FOLAlgorithms._
import domain.fol.Substitution._
import domain.fol.{DU, Substitution, Singleton}
import helpers.HelperFunctions
import scala.{Function => ScalaFun}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 31.03.2010
 * Time: 20:38:23
 */


object SubstitutionIndexTree {

  val reservedQueryVar = Variable("queryV")


  /* standard retrieval
  The search in the index is started for a si ngle substitution which in this context
  is called query. In contrast to the merge this is calles standard retrieval.

  Retrieval in Substitution trees is very simple. Generally, the retrieval algorithm checks each node of the tree for some special conditions. If the conditions are fulfilled the algorithm proceeds with the subnodes of the node that has been successfully tested. On its way down to the leaf nodes of the tree, the set of passed nodes is collected.
  There are three different tests which have to be performed:
  Find more general substitutions, compatible(/unifiable) substitutions, and instances.
  The functions Q, 1, and U Support the tests at the nodes of the tree.
  For each assignment Xi i-> U of the current node's Substitution the functions test whether the variable Xi or whatever it is
  bound to is more general, an instance of, or unifiable with t,-.
  Each of these functions can be used as a Parameter for the retrieval function	search.
  */
  def search(tree: SubstitutionIndexTree, bindingStack: scala.collection.mutable.Stack[Tuple2[Variable, FOLNode]], testFunction: (SubstitutionIndexTree, Stack[Tuple2[Variable, FOLNode]]) => Option[Int]): List[SubstitutionIndexTree] = {
    var hits: List[SubstitutionIndexTree] = List()

    // if the test was successful , we get a binding count back ( if bindings have been established)
    // 0 if the test was trivial
    tree match {
      case EmptyTree() =>
      case _ => {
        testFunction(tree, bindingStack) match {
          case Some(bindingsCount) => {
            // add current node as a hit
            hits = tree :: hits

            // descend
            for (child <- tree.children) {
              hits = hits ::: search(child, bindingStack, testFunction)
            }

            // backtrack
            for (i <- 0 until bindingsCount) {
              bindingStack.pop
            }


          }

          case None => {
            // descend
            for (child <- tree.children) {
              hits = hits ::: search(child, bindingStack, testFunction)
            }

          }

        }

      }
    }
                   

    hits

  }



  // pool of used variable names
  val testUnification = (tree: SubstitutionIndexTree, stack: Stack[Tuple2[Variable, FOLNode]]) => {
    unifiers(tree.substitution.get, stack) match {
      case Some(unifier) => {
        for (binding <- unifier)
          stack.push(binding)
        Some(unifier.size)
      }
      case None => {
        None
      }

    }

  }

  val testInstantiation = (tree: SubstitutionIndexTree, stack: Stack[Tuple2[Variable, FOLNode]]) => {
    instances(tree.substitution.get, stack) match {
      case Some(instantiator) => {
        for (binding <- instantiator)
          stack.push(binding)
        Some(instantiator.size)
      }
      case None => {
        None
      }

    }

  }

  val testGeneralization = (tree: SubstitutionIndexTree, stack: Stack[Tuple2[Variable, FOLNode]]) => {
    generalizations(tree.substitution.get, stack) match {
      case Some(generalizer) => {
        for (binding <- generalizer)
          stack.push(binding)
        Some(generalizer.size)
      }
      case None => {
        None
      }

    }

  }


  /**
   * The insertion heuristic is used for descrending into the tree. The heuristic has to master 3 different cases :
  ¥	Either it selects a variant subnode of the current node for descending or
  ¥	It selects a non-variant subnode, which will yield a non empty mscg, if a variant could not be found
  ¥	Or if such a node could not be found either , it selects the empty tree , in which case the insertion function creates a new leaf node.

  First Fit Heuristic
  In our implementation we use a very simple firt-fit heuristic : We choose the first variant son for descending, if such a node does not exist
  the first non-variant son that produces a non-empty mscg is selected
   */
  def firstFitHeuristic(node: SubstitutionIndexTree, p: Substitution): SubstitutionIndexTree = {
    // find first variant son
    var found: SubstitutionIndexTree = EmptyTree()

    findVariantSubNode(node, p) match {
      case Some(firstVariantSon) => {
        found = firstVariantSon
      }

      case None => {
        // there are no variant children , get the first node that produces a non-empty mscg
        findFirstNonEmptyMSCGGeneratingSubNode(node, p) match {
          case Some(nonVariantSon) => {
            found = nonVariantSon
          }

          case None => {
            found = EmptyTree()
          }
        }

      }
    }

    //log.debug("FirstFirHeuristic determined Node : %s as next node for currentNode : %s " format (found, node))
    found

  }


  def findFirstNonEmptyMSCGGeneratingSubNode(node: SubstitutionIndexTree, p: Substitution) = {

    //    for (child <- node.children) {
    //      // get the variant matcher for root and p
    //      val vs = variants(node.substitution.get, p).get
    //      val r = child.substitution.get
    //      val l = LMSCG(r, (p compose vs))
    //      log.debug("MSCG for %s and %s is %s" format (r, p, l))
    //    }

    node.children.find({
      child: SubstitutionIndexTree => (child.substitution, p) match {
        case (Some(s1), s2) => {
          val r = node.substitution.get
          val childSub: Substitution = s1
          val p = s2

          // take head of p , dont intrested in rest ... TODO check tthis
          val l = LMSCG((r join childSub), p.toList.head,Nil)
          l match {
            case Some((generalization, specialization1, specialization2)) if (!generalization.isEmpty) => {
              val t = (generalization, specialization1, specialization2)
              true
            }
            case _ => {
              false
            }
          }

        }
        case _ => {
          false
        }
      }
    })


  }


  def findVariantSubNode(node: SubstitutionIndexTree, p: Substitution) = {
//    for (child <- node.children) {
//      // get the variant matcher for root and p
//      val vs = variants(node.substitution.get, p).get
//      val r = child.substitution.get
//      val isVariantSubnode = isVariantSubs(r, (p compose vs))
//      if (isVariantSubnode)
//        log.debug("Substitution %s IS A variant substitution for %s", r, p)
//      else
//        log.debug("Substitution %s is NOT a variant substitution for %s" ,r, p)
//    }

    node.children.find({
      child: SubstitutionIndexTree => {
        // get the variant matcher for root and p
        val vs = variants(node.substitution.get, p).get
        val r = child.substitution.get
        val isVariantSubnode = isVariantSubs(r, (p compose vs))
        isVariantSubnode
      }
    })

  }

  def isVariantSubNode(node1: SubstitutionIndexTree, node2: SubstitutionIndexTree) = {
    (node1.substitution, node2.substitution) match {
      case (Some(s1), Some(s2)) if (s1 != s2) => {
        isVariantSubs(s1, s2)
      }
      case _ => {
        false
      }
    }
  }


  def isVariantSubs(r: Substitution, p: Substitution): Boolean = {
    variants(r, p) match {
      case Some(vs) if (!vs.isEmpty) => {
        true
      }
      case Some(vs) if (vs.isEmpty) => { // trivial variant matcher
        true
      }
      case _ => {
        false
      }
    }
  }

  def insert(tree: SubstitutionIndexTree, p: Substitution, term: FOLNode, openVariables: List[Variable]): SubstitutionIndexTree = {

    (tree, p) match {
      case (et: EmptyTree, p) => {
        //println("RULE 6.14 FIRED")
        // rule 6.14 p173 Term Indexing - Peter Graf ( LNCS 1053 )
        // a new leaf node is created
        LeafNode(p, List(term),p.domain)

      }

      case (LeafNode(r,terms,bav), p) if (isVariantSubs(r, p)) => {
        //println("RULE 6.15 FIRED")
        // rule 6.15 -- An existing leaf node is returned if p corresponds to a variant
        // substitution which has already been inserted
        tree
      }

      // inner nodes :
      case (InnerNode(r, children,bav), p) if ((isVariantSubs(r, p) && (firstFitHeuristic(tree, p) match {
        case EmptyTree() => false
        case _ => true
      }))) => {
        //println("RULE 6.16 FIRED")
        val nextNode = firstFitHeuristic(tree, p)
        val otherChildren = children - nextNode
        val vs = variants(r, p).get

        // 6.16 -- If a variant node is found AND heuristic found a child node
        InnerNode(r, List(insert(nextNode, p compose vs, term, r.image ++ (openVariables -- r.domain))) ::: otherChildren,r.domain)

      }

      case (InnerNode(r, children,bav), p) if ((isVariantSubs(r, p) && (firstFitHeuristic(tree, p) match {
        case EmptyTree() => true
        case _ => false
      }))) => { // 6.17
        // Inner node , and the heuristic returned the empty tree --> a new leaf node is created
        val vs = variants(r, p).get
        val restr = r.image ++ (openVariables -- r.domain)
        val leafSub = (p compose vs).restrict(restr)
        InnerNode(r, List(LeafNode(leafSub, List(term),bav ++ leafSub.domain)) ::: children,r.domain) // TODO RESTRICTION IS USED HERE !!!
      }

      case (InnerNode(r, children,bav), p) if (LMSCG(r, p,bav).isDefined) => { // 6.18
        // non variant node
        // create new innernode and a new leaf node
        val (mu, s1, s2) = LMSCG(r, p,bav).get

        val restr = (openVariables -- r.domain)
        val rightChildSub = s2 + p.restrict(restr)
        InnerNode(mu, List(InnerNode(s1, children,bav ++ mu.domain ++ s1.domain), LeafNode(rightChildSub, List(term),bav ++ mu.domain ++ rightChildSub.domain)),bav) // p is restricted here

      }

      case (LeafNode(r, terms,bav), p) if (LMSCG(r, p,bav).isDefined) => { // 6.18
        // non variant node
        // create new innernode and a new leaf node
        val (mu, s1, s2) = LMSCG(r, p,bav).get

        val restr = (openVariables -- r.domain)
        val prestr = p.restrict(restr)
        val rightChildSub = s2 + prestr
        InnerNode(mu, List(LeafNode(s1, terms,bav ++ mu.domain ++ s1.domain), LeafNode(rightChildSub, List(term),bav ++ mu.domain ++ rightChildSub.domain)),bav) // p is restricted here

      }


      case _ => {
        if (isVariantSubs(tree.substitution.get, p))
          error("Variant node found but not matched correctly")
        else if (isVariantSubs(p,tree.substitution.get))
          error("INVERSE Variant found??")
        else {
          error("No Match!")
        }

      }


    }


    // continue with the child note selected by the heuristic
    // TODO -- as the index is used as a means to accessing data, it should be possible to
    // store additional information at the leaf nodes of the tree. Therefore, in an implementation
    // of insert it is reasonable to return the found or created leaf node, such that the
    // user can perform additional operations.



  }


}


sealed trait SubstitutionIndexTree {
  // substition saved in the current node
  val substitution: Option[Substitution]


  val boundAuxiliaryVariables : List[Variable]

  val children: List[SubstitutionIndexTree]

  def substitutions: Set[Substitution] = {
    // substititutions savad in the substrees
    this match {
      case LeafNode(s, _,_) => Set(s)
      case InnerNode(s, subtrees,_) => {
        subtrees.map(_.substitutions).foldLeft(Set(s))(_ ++ _)
      }
      case t: EmptyTree => {
        Set()
      }
    }
  }


  def retrieveUnifiable(query: Substitution) = {
    // init the stack
    val bindingStack: scala.collection.mutable.Stack[Tuple2[Variable, FOLNode]] = new Stack

    for (binding <- query)
      bindingStack.push(binding)

    val hits = SubstitutionIndexTree.search(this, bindingStack, SubstitutionIndexTree.testUnification)
    hits

  }


  def retrieveInstances(query: Substitution) = {
    // init the stack
    val bindingStack: scala.collection.mutable.Stack[Tuple2[Variable, FOLNode]] = new Stack

    for (binding <- query)
      bindingStack.push(binding)

    val hits = SubstitutionIndexTree.search(this, bindingStack, SubstitutionIndexTree.testInstantiation)
    hits

  }

  def retrieveGeneral(query: Substitution) = {
    // init the stack
    val bindingStack: scala.collection.mutable.Stack[Tuple2[Variable, FOLNode]] = new Stack

    for (binding <- query)
      bindingStack.push(binding)

    val hits = SubstitutionIndexTree.search(this, bindingStack, SubstitutionIndexTree.testGeneralization)
    hits

  }


  def insert(term: FOLNode): SubstitutionIndexTree = {
    // create substitution
    // TODO FIXME this stinks
    val s = Substitution(Map(Variable("queryV") -> term))
    // normalizet he substition
    val ns = s.normalize
    SubstitutionIndexTree.insert(this, ns, term, s.domain)
  }

}


case class EmptyTree() extends SubstitutionIndexTree {
  override val boundAuxiliaryVariables = Nil
  override val children = Nil
  override val substitution = None
}

// A leaf node.
case class LeafNode(s: Substitution, val terms: List[FOLNode],override val boundAuxiliaryVariables : List[Variable]) extends SubstitutionIndexTree {
  // add the variables of s domain to the already bound auxvariables
  override val substitution = Some(s)
  override val children = Nil

}


// A inner node.
case class InnerNode(s: Substitution, override val children: List[SubstitutionIndexTree],override val boundAuxiliaryVariables : List[Variable]) extends SubstitutionIndexTree {
  override val substitution = Some(s)
}



object VariantNode {
  def unapply(tuple: Tuple2[SubstitutionIndexTree, Substitution]): Option[Tuple2[SubstitutionIndexTree, Substitution]] = {
    tuple match {
      case (InnerNode(r, children,_), p) => {
        // check if there is a variabliy substituion
        variants(r, p) match {
          case Some(variantMatcher) => Some((tuple._1, variantMatcher))
          case None => None
        }
      }

      case _ => None
    }
  }
}

object VariantLeafNode {
  def isTuple(ar: AnyRef, n: Int) = {   // TODO this is handy utlity method , move to helpers
    val name = """scala\.Tuple(\d\d?)""".r.unapplySeq(ar.getClass.getName)
    name.map(_.map(_.toInt).exists(i => i >= n && i < 23)).getOrElse(false)
  }

  def unapply(a: Any): Option[(SubstitutionIndexTree, Substitution, Substitution)] = {
    if (isTuple(a.asInstanceOf[AnyRef], 2)) {
      val prod = a.asInstanceOf[Product]

      val tree = prod.productElement(0).asInstanceOf[SubstitutionIndexTree]
      val p = prod.productElement(1).asInstanceOf[Substitution]

      (tree, p) match {
        case (LeafNode(r, _,_), p) => {
          //println("Unapplying VAriantLeafNode Extractor")
          // check if there is a variabliy substituion
          variants(r, p) match {
            case Some(variantMatcher) => Some((tree, variantMatcher, p))
            case None => None
          }
        }

        case _ => {
          //println("Unapplying VAriantLeafNode Extractor")
          None
        }
      }


    }
    else None
  }


}




object ARG {
  val arg = (X: List[FOLNode], Y: List[FOLNode], s1: Substitution, s2: Substitution, Z: List[FOLNode],bav : List[Variable]) => {
    (X, Y, s1, s2, Z,bav) match {
      case (tx :: xs, ty :: ys, s1, s2, Z,bav) => {
        val (tz, _s1, _s2) = LMSCG(tx, ty, s1, s2,bav)
        (xs, ys, _s1, _s2, Z ::: List(tz),bav)
      }

      case _ => {
        error("ARG transition function could not execute, arguments not in expected form") // TODO better error message
        (X, Y, s1, s2, Z,bav)
      }


    }
  }

  private def compatibleTransition(X: List[FOLNode], Y: List[FOLNode], s1: Substitution, s2: Substitution, Z: List[FOLNode],bav : List[Variable]) = {
    (X, Y, s1, s2, Z,bav) match {
    // arg
      case (tx :: xs, ty :: ys, s1, s2, Z,bav) => {
        val (tz, _s1, _s2) = LMSCG(tx, ty, s1, s2,bav)
        //println("FIRING RULE : ARG")
        Some(arg)
      }

      case _ => {
        None
      }


    }

  }


  def apply(X: List[FOLNode], Y: List[FOLNode], s1: Substitution, s2: Substitution, Z: List[FOLNode],bav : List[Variable]): Tuple6[List[FOLNode], List[FOLNode], Substitution, Substitution, List[FOLNode],List[Variable]] = {
    // as long as arg can be applied apply arg
    // FIXME 2.8 this can be simplified in scala 2.8
    // get the first MSSG transition function
    var tuple = (X, Y, s1, s2, Z,bav)
    var nextTransition = HelperFunctions.tupled(compatibleTransition _)(tuple)
    while (nextTransition.isDefined) { // while there is a transition function
      // apply the transition to the substitutions 5tuple
      tuple = HelperFunctions.tupled(nextTransition.get.apply _)(tuple)
      // get the mext MSSG transition function
      nextTransition = HelperFunctions.tupled(compatibleTransition _)(tuple)
    }

    // return tuple after all ARG transitions have been applied
    tuple
  }
}






object MSCG {
  val bind = (r: Substitution, p: Substitution, mu: Substitution, s1: Substitution, s2: Substitution,bav : List[Variable]) => {
    (r, p, mu, s1, s2,bav) match {
    // BIND
      case (Singleton(xi, t) DU r, p, mu, s1, s2,bav) => {
        (r, p, mu, s1 + (xi, t), s2,bav)
      }
      case _ => {
        error("BIND transition function could not execute, arguments not in expected form")
        (r, p, mu, s1, s2,bav)
      }
    }
  }

  val bindS = (r: Substitution, p: Substitution, mu: Substitution, s1: Substitution, s2: Substitution,bav : List[Variable]) => {
    (r, p, mu, s1, s2,bav) match {
    // BIND
      case (Singleton(xi, t) DU r, p, mu, s1, s2,bav) => {
        (r, p, mu, s1, s2 + (xi, t),bav)
      }
      case _ => {
        error("BIND transition function could not execute, arguments not in expected form")
        (r, p, mu, s1, s2,bav)
      }
    }
  }




  val freeze = (r: Substitution, p: Substitution, mu: Substitution, s1: Substitution, s2: Substitution,bav : List[Variable]) => {
    (r, p, mu, s1, s2,bav) match {

      case (Singleton(xi, t) DU r, p, mu, s1, s2,bav) if (p.binding(xi) == t) => {
        (r, p, mu + (xi, t), s1, s2,bav)
      }

      case _ => {
        error("FREEZE transition function could not execute, arguments not in expected form")
        (r, p, mu, s1, s2,bav)
      }


    }
  }

  val divide = (r: Substitution, p: Substitution, mu: Substitution, s1: Substitution, s2: Substitution,bav : List[Variable]) => {
    (r, p, mu, s1, s2,bav) match {
      case (Singleton(xi, t) DU r, p, mu, s1, s2,bav) if (p.binding(xi).top != t.top) => {
        (r, p, mu, s1 + (xi, t), s2 + (xi, p.binding(xi)),bav)
      }

      case _ => {
        error("DIVIDE transition function could not execute, arguments not in expected form")
        (r, p, mu, s1, s2,bav)
      }

    }
  }

  val mix = (r: Substitution, p: Substitution, mu: Substitution, s1: Substitution, s2: Substitution,bav : List[Variable]) => {
    (r, p, mu, s1, s2,bav) match {
      case (Singleton(xi, Function(topF, argsF)) DU r, p, mu, s1, s2,bav)
        if (p.binding(xi).top == topF &&
                p.binding(xi).args != argsF &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._1.isEmpty &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._2.isEmpty) => { // OMG Complexity alarm !!

        val (_, _, _s1, _s2, argsZ,_) = ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)

        (r, p, mu + (xi, Function(topF, argsZ)), _s1, _s2,bav)

      }

      case (Singleton(xi, Predicate(topF, argsF)) DU r, p, mu, s1, s2,bav)
        if (p.binding(xi).top == topF &&
                p.binding(xi).args != argsF &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._1.isEmpty &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._2.isEmpty) => { // OMG Complexity alarm !!

        val (_, _, _s1, _s2, argsZ,_) = ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)

        (r, p, mu + (xi, Predicate(topF, argsZ)), _s1, _s2,bav)

      }
      case _ => {
        error("MIX transition function could not execute, arguments not in expected form")
        (r, p, mu, s1, s2,bav)
      }


    }
  }


  def apply(r: Substitution, p: Substitution, mu: Substitution, s1: Substitution, s2: Substitution,bav : List[Variable]) = {
    require(!r.isEmpty)

    var subsTuple = (r, p, Substitution(), Substitution(), Substitution(),bav)
    // FIXME 2.8 this can be simplified in scala 2.8
    // get the first MSSG transition function
    var nextTransition = HelperFunctions.tupled(compatibleTransition _)(subsTuple)

    while (nextTransition.isDefined) { // while there is a transition function
      // apply the transition to the substitutions 5tuple
      subsTuple = HelperFunctions.tupled(nextTransition.get.apply _)(subsTuple)
      // get the mext MSSG transition function
      nextTransition = HelperFunctions.tupled(compatibleTransition _)(subsTuple)
    }

    // return the 5Tuple after all possbile transformations
    subsTuple


  }


  private def compatibleTransition(r: Substitution, p: Substitution, mu: Substitution, s1: Substitution, s2: Substitution,bav : List[Variable]) = {
    // check which tranistion function to return , or None if there is no compatible
    (r, p, mu, s1, s2,bav) match {

    // BIND
      case (Singleton(xi, t) DU r, p, mu, s1, s2,bav) if (!p.domain.contains(xi)) => {
        //println("FIRING RULE : BIND")
        Some(bind)
      }




      // FREEZE
      case (Singleton(xi, t) DU r, p, mu, s1, s2,bav) if (p.binding(xi) == t) => {
        //println("FIRING RULE : FREEZE")
        val pxi = p.binding(xi)
        Some(freeze)
      }

      // DIVIDE
      case (Singleton(xi, t) DU r, p, mu, s1, s2,bav) if (p.binding(xi).top != t.top) => {
        //println("FIRING RULE : DIVIDE")
        Some(divide)
      }

      // MIX Function
      case (Singleton(xi, Function(topF, argsF)) DU r, p, mu, s1, s2,bav)
        if (p.binding(xi).top == topF &&
                p.binding(xi).args != argsF &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._1.isEmpty &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._2.isEmpty) => { // OMG !!
        //println("FIRING RULE : MIX")
        Some(mix)
      }

      // MIX Predicate
      case (Singleton(xi, Predicate(topF, argsF)) DU r, p, mu, s1, s2,bav)
        if (p.binding(xi).top == topF &&
                p.binding(xi).args != argsF &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._1.isEmpty &&
                ARG(argsF, p.binding(xi).args, s1, s2, List(),bav)._2.isEmpty) => { // OMG !!
        //println("FIRING RULE : MIX")
        Some(mix)
      }

      // BIND Specia;
      //      case (Singleton(xi, t) DU r, p, mu, s1, s2) => {
      //        //println("FIRING RULE : BINDSpecial")
      //        Some(bindS)
      //      }

      // MIX
      //      case (Singleton(xi, t) DU r, p, mu, s1, s2)
      //        if (p(xi).top == t.top &&
      //                p(xi).args != t.args &&
      //                ARG(t.args, p(xi).args, s1, s2, List())._1.isEmpty &&
      //                ARG(t.args, p(xi).args, s1, s2, List())._2.isEmpty) => { // OMG !!
      //        //println("FIRING RULE : MIX")
      //        Some(mix)
      //      }

      case _ => {
        //println("NO MORE RULES TO FIRE!")
        // NO more transitions
        None
      }
    }


  }

}


object LMSCG {
  def apply(term1: FOLNode, term2: FOLNode, s1: Substitution, s2: Substitution,bav : List[Variable]): Tuple3[FOLNode, Substitution, Substitution] = {
    (term1, term2, s1, s2,bav) match {
      case (term1, term2, s1, s2,bav) if (term1 == term2) => {
        //println("6.24 -- the mscg of two indetical terms is the term itself")
        // 6.24 -- the mscg of two indetical terms is the term itself
        (term1, s1, s2)
      }

//      case (NonIndicatorVariable(niv), term2, s1, s2,bav) => {
//        // 6.25 -- In case the first argument is a non indicator variable, this variable can
//        // be used in the specialisation for term t
//        //println("6.25 -- In case the first argument is a non indicator variable")
//        (niv, s1, s2 + (niv -> term2))
//      }

      case (Function(nameX, argsX), Function(nameY, argsY), s1, s2,bav) if (nameX == nameY &&
              ARG(argsX, argsY, s1, s2, List(),bav)._1.isEmpty &&
              ARG(argsX, argsY, s1, s2, List(),bav)._2.isEmpty) => {
        //println("6.26 -- the transition arg is used to process the arguments of identical top symbols")
        // 6.21 -- the transition arg is used to process the arguments of identical top symbols
        // by computing the normal form with respect to ARG. The reuse of assignments in the found
        // specializations is described in 6.22
        val a = ARG(argsX, argsY, s1, s2, List(),bav)
        a match {
          case (Nil, Nil, ss1, ss2, z,bav) => {
            val zArgs = z
            (Function(nameX, zArgs), ss1, ss2)

          }
          case _ => {
            error("Illegal Match here")
          }

        }


      }

      case (Predicate(nameX, argsX), Predicate(nameY, argsY), s1, s2,bav) if (nameX == nameY &&
              ARG(argsX, argsY, s1, s2, List(),bav)._1.isEmpty &&
              ARG(argsX, argsY, s1, s2, List(),bav)._2.isEmpty) => {
        //println("6.26 -- the transition arg is used to process the arguments of identical top symbols")
        // 6.21 -- the transition arg is used to process the arguments of identical top symbols
        // by computing the normal form with respect to ARG. The reuse of assignments in the found
        // specializations is described in 6.22
        val a = ARG(argsX, argsY, s1, s2, List(),bav)
        a match {
          case (Nil, Nil, ss1, ss2, z,bav) => {
            val zArgs = z
            (Predicate(nameX, zArgs), ss1, ss2)

          }
          case _ => {
            error("Illegal Match here")
          }

        }


      }


      // ! rule 6.22 omitted ==> only linear generalizations will be produced


      case (term1, term2, s1, s2,bav) => {
        // Finally new nonindicator variables are introduced in case none of the other rules are
        // could be applied

        // TODO this can be optimized by reusing auxilaiary variables already in the tree
        // TODO refer to Peter Graf - Term Indexing p. 178 "Reusing non indicator variables"

        // introduce new non indicator variable

        // check which variables have been already bound


        //log.info("Bounded auxiliary variables except query vars are : %s",bav)

//        val xj = Variable.nextAuxiliary(s1.domain ++ s2.domain ++ bav)
        val xj = Variable()
        val ds1 = s1
        val ds2 = s2
        (xj, s1 + (xj -> term1), s2 + (xj -> term2))
      }


    }

  }


  def apply(r: Substitution, p: Substitution,bav : List[Variable]): Option[Tuple3[Substitution, Substitution, Substitution]] = {
    // USE TRANSITION SYSMTE MSCG
    require(!r.isEmpty)

    MSCG(r, p, Substitution(), Substitution(), Substitution(),bav) match {
      case (s, p, mu, s1, s2,bav) if (s.isEmpty) => {
        Some(mu, s1, s2)
      }

      case _ => {
        None
      }
    }


  }

}



//
//// A helper object.
//object GenericTree {
//
//  // empty: Converts an orderable type into an empty RBMap.
//  def empty[K <: Ordered[K], V] : GenericTree[K,V] = L()((k : K) => k)
//
//  // apply: Assumes an implicit conversion.
//  def apply[K, V](args : (K,V)*) : GenericTree[K,V] = {
//    var currentMap : GenericTree[K,V] = L()
//    for ((k,v) <- args) {
//      currentMap = currentMap.insert(k,v)
//    }
//    currentMap
//  }
//}
