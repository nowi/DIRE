package core.ordering


import domain.fol.ast._
import org.slf4j.LoggerFactory

/**
 * User: nowi
 * Date: 29.10.2009
 * Time: 12:53:32
 */


/**
 * A comparison function, which imposes a <i>strict partial ordering</i> on some
 * collection of literals.
 *
 * In Contrast this to the default java.lang.Comparator that imposes a <i>total ordering</i>
 * the literal comparator cannot compare every thing an will return Nothing when applied on
 * objects that are not comparable (instead of throwing ClassCastExcetpion)
 *
 * @author Philipp Nowakowski
 * @see Comparator
 */
trait LiteralComparator {
  def compare(a: FOLNode, b: FOLNode): Option[Int]

  def isGreater(o1: FOLNode, o2: FOLNode): Boolean = {
    // compare , handle notcomparable as equal
    compare(o1, o2) match {
      case Some(result) => {
        if (result > 0)
          true
        else false
      }
      case None => false
    }

  }

}


/**
 * A lexicographic path ordering (LPO) is a term ordering induced by a well-founded strict
 * precedence > over function, predicate and logical symbols, deﬁned by:
 * s = f (s1 , . . . , sm) ≻ g(t1 , . . . , tn ) = t if and only if t is a proper subterm
 * of s or at least one of the following holds
 *  (i)  f > g and s ≻ ti for all i with 1 ≤ i ≤ n
 *  (ii) f = g and for some j we have (s1 , . . . , sj −1 ) = (t1 , . . . , tj −! ),
 *       sj ≻ tj and s ≻ tk for all k with j < k ≤ n (iii) sj ≽ t for some j with 1 ≤ j ≤ m
 *
 * LPOs have the subterm property, i.e. t ≻ t′ for all terms t′ that are subterms of term t.
 * Furthermore, if > is total, the LPO induced by > is total on ground terms.
 *
 *
 */
//class LPOComparator extends LiteralComparator {
//  val log = LoggerFactory getLogger (this getClass)
//
//  def compare(o1: FOLNode, o2: FOLNode) = {
//    // compare , handle notcomparable as equal
//    comparePartial(o1, o2) match {
//      case Some(result) => result
//      case None => 0
//    }
//
//  }
//
//  def isGreater(o1: FOLNode, o2: FOLNode): Boolean = {
//    // compare , handle notcomparable as equal
//    comparePartial(o1, o2) match {
//      case Some(result) => {
//        if (result > 0)
//          true
//        else false
//
//      }
//      case None => false
//    }
//
//  }
//
//
//
//  //  s = f (s1 , . . . , sm) ≻ g(t1 , . . . , tn ) = t if and only if t is a proper subterm
//  // * of s or at least one of the following holds
//  // *  (i)  f > g and s ≻ ti for all i with 1 ≤ i ≤ n
//  // *  (ii) f = g and for some j we have (s1 , . . . , sj −1 ) = (t1 , . . . , tj −! ),
//  // *       sj ≻ tj and s ≻ tk for all k with j < k ≤ n (iii) sj ≽ t for some j with 1 ≤ j ≤ m
//  def comparePartial(s: FOLNode, t: FOLNode): Option[Int] = {
//    //  this = f (s1 , . . . , sm) ≻ g(t1 , . . . , tn ) = that if and only if that is a proper subterm
//    if (s == t) {
//      Some(0)
//    } else if (s.containsSubterm(t)) {
//      log.trace("{} contains {} as subterm", s, t)
//      Some(1)
//    } else {
//      // distinct the possible cases
//      (s, t) match {
//        case (Nary(f), Nary(g)) => {
//          // f and g are n-ary
//          // f > g and s ≻ ti for all i with 1 ≤ i ≤ n
//          log.trace("{} is N-Ary {} is N-Ary", f, g)
//
//          // test the rules
//          val isGreater = rule1(f, g) || rule2(f, g) || rule3(f, g)
//          val isSmaller = rule1(g, f) || rule2(g, f) || rule3(g, f)
//
//          if (isGreater) {
//            Some(1)
//          } else if (isSmaller) {
//            Some(-1)
//          } else {
//            log.trace("Could not comparePartial {} with {}", g, f)
//            None
//          }
//
//        }
//
//        case (Nary(f), Unary(g)) => {
//          log.trace("{} is N-Ary {} is UNAry", f, g)
//          Some(1)
//        }
//        case (Unary(f), Nary(g)) => {
//          log.trace("{} is N-Ary {} is UNAry", g, f)
//          Some(-1)
//        }
//        case (Unary(f), Unary(g)) => {
//          log.trace("{} is UNAry {} is UNAry", g, f)
//          Some(comparePrecedence(f, g))
//        }
//      }
//
//
//    }
//
//  }
//
//
//  def compareArgs(f: FOLNode, g: FOLNode): Option[Int] = {
//    //  f = g and for some j we have (s1 , . . . , sj-1) = (t1 , . . . , tj-1),
//    // sj ≻ tj
//    // and s ≻ tk for all k with j < k ≤ n
//    var result: Option[Int] = None
//    val lengthD = f.args.length - g.args.length
//    if (lengthD == 0) {
//      for (j <- 0 until g.args.length) {
//        val sj = f.args(j)
//        val tj = g.args(j)
//        // same or sj > tj
//        result = comparePartial(sj, tj) match {
//          case Some(r) => {
//            if (r >= 0) {
//              // and s ≻ tk for all k with j < k ≤ n
//              var isRestSmaller: Boolean = false
//              for (k <- j until g.args.length) {
//                val tk = g.args(k)
//                if (comparePartial(f, tk).getOrElse(0) > 0) isRestSmaller = true
//              }
//              if (isRestSmaller) {
//                Some(1)
//              } else None
//
//            } else None
//
//          }
//          case None => None
//        }
//
//      }
//
//    } else if (lengthD < 0) {
//      result = Some(-1)
//
//    } else {
//      result = Some(1)
//    }
//
//
//    result match {
//      case None => log.trace("Could not comparePartial f : {} with g : {}", f, g)
//      case _ => result
//    }
//
//    result
//
//  }
//
//
//  def rule1(f: FOLNode, g: FOLNode): Boolean = {
//    // f > g and s ≻ ti for all i with 1 ≤ i ≤ n
//    val result = (comparePrecedence(f, g) == 1 && g.args.forall(comparePartial(f, _).getOrElse(false) == 1))
//    log.trace("RULE 1 evalutaes to {} ", result)
//    result
//  }
//
//
//  def rule2(f: FOLNode, g: FOLNode): Boolean = {
//    //    (ii) f = g and for some j we have (s1 , . . . , sj−1 ) = (t1 , . . . , tj −! ),
//    // sj tj and s tk for all k with j < k ≤ n
//    val result = (comparePrecedence(f, g) == 0 && compareArgs(f, g).getOrElse(false) == 1)
//    log.trace("RULE 2 evalutaes to {} ", result)
//    result
//
//  }
//
//  def rule3(f: FOLNode, g: FOLNode): Boolean = {
//    // sj ≽ t for some j with 1 ≤ j ≤ m
//    val result = f.args.exists(comparePartial(_, g).getOrElse(false) == 1)
//    log.trace("RULE 3 evalutaes to {} ", result)
//    result
//  }
//
//
//  def comparePrecedence(a: FOLNode, b: FOLNode): Int = {
//    // compare lexical
//    val result = a.asInstanceOf[Term].name.compareTo(b.asInstanceOf[Term].name)
//    //    log.trace("Precedence comparison for {} and {} yielded %d", a, b, result)
//
//    if (result > 0) {
//      log.trace(" {} has higher precedence than {} ", a, b)
//      1
//    } else if (result < 0) {
//      log.trace(" {} has higher precedence than {} ", b, a)
//      -1
//    } else {
//      log.trace(" {} has same precedence as {} ", a, b)
//      0
//    }
//
//  }
//
//

/**
 * ALCLPOOrdering
 *
 * For the simple types of literals occurring in clauses translated from ALC axioms
 * a LPO ordering with precedence as required by Deﬁnition 7 breaks
 * down to a simple ordering deﬁnition. Literals that contain different variables are incomparable,
 * for literals that share the same variables the ordering is determined by three rules:
 *  1.) Literals containing a function symbol precede literals that do not contain a function symbol.
 *  2.) Literals containing a function symbol are ordered according to the precedence of the function symbols.
 *  3.) Literals not containing a function symbol are ordered according to the precedence of the predicate symbols.
 */
class ALCLPOComparator extends LiteralComparator {
  val log = LoggerFactory getLogger (this getClass)


  def compare(a: FOLNode, b: FOLNode) = {
    //    Literals that contain different variables are incomparable

    //
    val aVars = a.flatArgs.filter({_.isInstanceOf[Variable]})
    val bVars = b.flatArgs.filter({_.isInstanceOf[Variable]})


    if (aVars != bVars) {
      //    Literals that contain different variables are incomparable
      log.info("The literals {} and {} are incomparable", a, b)
      None
    } else {
      // 1.) Literals containing a function symbol precede literals that do not contain a function symbol.
      val aFuns = a.flatArgs.filter({_.isInstanceOf[Function]})
      val bFuns = b.flatArgs.filter({_.isInstanceOf[Function]})
      // first compare if any of the lits does not contain a function symbol
      (aFuns, bFuns) match {
      // 1.) Literals containing a function symbol precede literals that do not contain a function symbol.
        case (Nil, bfs :: bFuns) => Some(1)
        case (afs :: aFuns, Nil) => Some(-1)
        // 2.) Literals containing a function symbol are ordered according to the precedence of the function symbols.
        case (afs :: aFuns, bfs :: bFuns) => {
          // assert that there is only one function symbol per literal
          // --> no nesting
          assert(aFuns.size == 1 && bFuns.size == 1)
          Some(comparePrecedence(afs, bfs))
        }
        // 3.) Literals not containing a function symbol are ordered according to the precedence of the predicate symbols.
        case (Nil, Nil) => {
          val aPreds = a.flatArgs.filter({_.isInstanceOf[Predicate]})
          val bPreds = b.flatArgs.filter({_.isInstanceOf[Predicate]})

          (aPreds, bPreds) match {
          // 1.) Literals containing a predicate symbol precede literals that do not contain a predicate symbol.
            case (Nil, bps :: bPreds) => Some(1)
            case (aps :: aPreds, Nil) => Some(-1)
            // 2.) Literals containing a predicate symbol are ordered according to the precedence of the predicate symbols.
            case (aps :: aPreds, bps :: bPreds) => {
              // assert that there is only one predicate symbol per literal
              // --> no nesting
              assert(aPreds.size == 1 && bPreds.size == 1)
              Some(comparePrecedence(aps, bps))
            }
            // 3.) Literals not containing a predaice must now be varisbles or constants , comparePrecedence
            case (Nil, Nil) => Some(comparePrecedence(a, b))

          }

        }

        case _ => {
          log.error("Could not compare {} and {} , this seems not right")
          None
        }
      }


    }

  }

  def comparePrecedence(a: FOLNode, b: FOLNode): Int = {
    // compare lexical
    val result = a.asInstanceOf[Term].name.compareTo(b.asInstanceOf[Term].name)
    //    log.trace("Precedence comparison for {} and {} yielded %d", a, b, result)

    if (result > 0) {
      log.trace(" {} has higher precedence than {} ", a, b)
      1
    } else if (result < 0) {
      log.trace(" {} has higher precedence than {} ", b, a)
      -1
    } else {
      log.trace(" {} has same precedence as {} ", a, b)
      0
    }

  }


}