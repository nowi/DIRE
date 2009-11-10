package core.ordering


import domain.fol.ast._
import java.util.Comparator
import org.slf4j.LoggerFactory

/**
 * User: nowi
 * Date: 29.10.2009
 * Time: 12:53:32
 */

abstract class LiteralComparator extends Comparator[FOLNode] {
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
 * For the simple types of literals occurring in clauses translated from ALC axioms
 * (Table 1 described below), a LPO ordering with precedence as required by Deﬁnition 7 breaks
 * down to a simple ordering deﬁnition. Literals that contain different variables are incomparable,
 * for literals that share the same variables the ordering is determined by three rules:
 *  1.) Literals containing a function symbol precede literals that do not contain a function symbol.
 *  2.) Literals containing a function symbol are ordered according to the precedence of the function symbols.
 *  3.) Literals not containing a function symbol are ordered according to the precedence of the predicate symbols.

 *
 */
class ALCLPComparator extends LiteralComparator {
  val log = LoggerFactory getLogger (this getClass)

  def compare(o1: FOLNode, o2: FOLNode) = {
    // compare , handle notcomparable as equal
    comparePartial(o1, o2) match {
      case Some(result) => result
      case None => 0
    }

  }

  def isGreater(o1: FOLNode, o2: FOLNode): Boolean = {
    // compare , handle notcomparable as equal
    comparePartial(o1, o2) match {
      case Some(result) => {
        if (result > 0)
          true
        else false

      }
      case None => false
    }

  }



  //  s = f (s1 , . . . , sm) ≻ g(t1 , . . . , tn ) = t if and only if t is a proper subterm
  // * of s or at least one of the following holds
  // *  (i)  f > g and s ≻ ti for all i with 1 ≤ i ≤ n
  // *  (ii) f = g and for some j we have (s1 , . . . , sj −1 ) = (t1 , . . . , tj −! ),
  // *       sj ≻ tj and s ≻ tk for all k with j < k ≤ n (iii) sj ≽ t for some j with 1 ≤ j ≤ m
  def comparePartial(s: FOLNode, t: FOLNode): Option[Int] = {
    //  this = f (s1 , . . . , sm) ≻ g(t1 , . . . , tn ) = that if and only if that is a proper subterm
    if (s == t) {
      Some(0)
    } else if (s.containsSubterm(t)) {
      log.trace("{} contains {} as subterm", s, t)
      Some(1)
    } else {
      // distinct the possible cases
      (s, t) match {
        case (Nary(f), Nary(g)) => {
          // f and g are n-ary
          // f > g and s ≻ ti for all i with 1 ≤ i ≤ n
          log.trace("{} is N-Ary {} is N-Ary", f, g)

          // test the rules
          val isGreater = rule1(f, g) || rule2(f, g) || rule3(f, g)
          val isSmaller = rule1(g, f) || rule2(g, f) || rule3(g, f)

          if (isGreater) {
            Some(1)
          } else if (isSmaller) {
            Some(-1)
          } else {
            log.trace("Could not comparePartial {} with {}", g, f)
            None
          }

        }

        case (Nary(f), Unary(g)) => {
          log.trace("{} is N-Ary {} is UNAry", f, g)
          Some(1)
        }
        case (Unary(f), Nary(g)) => {
          log.trace("{} is N-Ary {} is UNAry", g, f)
          Some(-1)
        }
        case (Unary(f), Unary(g)) => {
          log.trace("{} is UNAry {} is UNAry", g, f)
          Some(comparePrecedence(f, g))
        }
      }


    }

  }


  def compareArgs(f: FOLNode, g: FOLNode): Option[Int] = {
    //  f = g and for some j we have (s1 , . . . , sj-1) = (t1 , . . . , tj-1),
    // sj ≻ tj
    // and s ≻ tk for all k with j < k ≤ n
    var result: Option[Int] = None
    val lengthD = f.args.length - g.args.length
    if (lengthD == 0) {
      for (j <- 0 until g.args.length) {
        val sj = f.args(j)
        val tj = g.args(j)
        // same or sj > tj
        result = comparePartial(sj, tj) match {
          case Some(r) => {
            if (r >= 0) {
              // and s ≻ tk for all k with j < k ≤ n
              var isRestSmaller: Boolean = false
              for (k <- j until g.args.length) {
                val tk = g.args(k)
                if (comparePartial(f, tk).getOrElse(0) > 0) isRestSmaller = true
              }
              if (isRestSmaller) {
                Some(1)
              } else None

            } else None

          }
          case None => None
        }

      }

    } else if (lengthD < 0) {
      result = Some(-1)

    } else {
      result = Some(1)
    }


    result match {
      case None => log.trace("Could not comparePartial f : {} with g : {}", f, g)
      case _ => result
    }

    result

  }


  def rule1(f: FOLNode, g: FOLNode): Boolean = {
    // f > g and s ≻ ti for all i with 1 ≤ i ≤ n
    val result = (comparePrecedence(f, g) == 1 && g.args.forall(comparePartial(f, _).getOrElse(false) == 1))
    log.trace("RULE 1 evalutaes to {} ", result)
    result
  }


  def rule2(f: FOLNode, g: FOLNode): Boolean = {
    //    (ii) f = g and for some j we have (s1 , . . . , sj−1 ) = (t1 , . . . , tj −! ),
    // sj tj and s tk for all k with j < k ≤ n
    val result = (comparePrecedence(f, g) == 0 && compareArgs(f, g).getOrElse(false) == 1)
    log.trace("RULE 2 evalutaes to {} ", result)
    result

  }

  def rule3(f: FOLNode, g: FOLNode): Boolean = {
    // sj ≽ t for some j with 1 ≤ j ≤ m
    val result = f.args.exists(comparePartial(_, g).getOrElse(false) == 1)
    log.trace("RULE 3 evalutaes to {} ", result)
    result
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