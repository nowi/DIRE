package core.ordering


import domain.fol.ast._
import net.lag.logging.Logger

/**
 * User: nowi
 * Date: 29.10.2009
 * Time: 12:53:32
 */

abstract class LiteralComparator {
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
  val log: Logger = Logger.get
  //  s = f (s1 , . . . , sm) ≻ g(t1 , . . . , tn ) = t if and only if t is a proper subterm
  // * of s or at least one of the following holds
  // *  (i)  f > g and s ≻ ti for all i with 1 ≤ i ≤ n
  // *  (ii) f = g and for some j we have (s1 , . . . , sj −1 ) = (t1 , . . . , tj −! ),
  // *       sj ≻ tj and s ≻ tk for all k with j < k ≤ n (iii) sj ≽ t for some j with 1 ≤ j ≤ m
  def compare(s: FOLNode, t: FOLNode): Int = {


    def compareArgs(f: FOLNode, g: FOLNode): Integer = {
      //  f = g and for some j we have (s1 , . . . , sj-1) = (t1 , . . . , tj-1),
      // sj ≻ tj
      // and s ≻ tk for all k with j < k ≤ n
      var result: Integer = -2
      for (j <- 0 until g.args.length) {
        val sj = f.args(j)
        val tj = g.args(j)
        // same or sj > tj
        if (compare(sj, tj) >= 0) {
          // and s ≻ tk for all k with j < k ≤ n
          var isRestSmaller: Boolean = false
          for (k <- j until g.args.length) {
            val tk = g.args(k)
            if (compare(f, tk) > 0) isRestSmaller = true
          }
          if (isRestSmaller) {
            result = 1
          }

        }

      }

      if (result == -2) {
        // no comparision was possible
        log.warning("Could not compare f : %s with g : %s", f, g)
      }

      result

    }

    //  this = f (s1 , . . . , sm) ≻ g(t1 , . . . , tn ) = that if and only if that is a proper subterm
    if (s == t) {
      0
    } else if (s.containsSubterm(t)) {
      log.info("%s contains %s as subterm", s, t)
      1
    } else {
      // distinct the possible cases
      (s, t) match {
        case (Nary(f), Nary(g)) => {
          // f and g are n-ary
          // f > g and s ≻ ti for all i with 1 ≤ i ≤ n
          log.info("%s is N-Ary %s is N-Ary", f, g)
          if (comparePrecedence(f, g) == 1 && g.args.forall(compare(f, _) == 1)) {
            log.info("%s has higher precedence then %s and is greater than all args of %s", f, g, g)
            1
          } else if (comparePrecedence(f, g) == 1 && g.args.forall(compare(f, _) == 0)) {
            log.info("%s has higher precedence then %s and all args are equal", f, g)
            1
          } else if (comparePrecedence(g, f) == 1 && f.args.forall(compare(_, g) == 1)) {
            log.info("%s has higher precedence then %s and is greater than all args of %s", g, f, f)
            -1
          } else if (comparePrecedence(g, f) == 1 && f.args.forall(compare(g, _) == 1)) {
            log.info("%s has higher precedence then %s and is greater than all args of %s", g, f, f)
            -1
          } else if (comparePrecedence(g, f) == 1 && f.args.forall(compare(g, _) == 0)) {
            log.info("%s has higher precedence then %s and all args are equal", g, f)
            -1
          } else if (comparePrecedence(f, g) == 1 && g.args.forall(compare(_, f) == 1)) {
            log.info("%s has higher precedence then %s and is greater than all args of %s", f, g, g)
            1
          } else if (comparePrecedence(f, g) == 0 && compareArgs(f, g) == 1) {
            log.info("%s has same precedence then %s and args compare yielded 1", f, g)
            1
          } else if (comparePrecedence(g, f) == 0 && compareArgs(g, f) == 1) {
            log.info("%s has same precedence then %s and args compare yielded 1", g, f)
            -1
          } else {
            log.warning("Could not compare %s with %s", g, f)
            0
          }

        }
        //        case (Nary(f), Unary(g)) => {
        //          log.info("%s is N-Ary %s is UNAry", f, g)
        //          // (iii) sj ≽ t for some j with 1 ≤ j ≤ m
        //          if (f.args.exists(compare(_, g) >= 0)) {
        //            log.info("%s has some args that are greater or equal then %s", f, g)
        //            1
        //          } else {
        //            log.warning("Could not compare f : %s with g : %s", f, g)
        //            0
        //          }
        //
        //
        //        }
        //        case (Unary(f), Nary(g)) => {
        //          log.info("%s is N-Ary %s is UNAry", g, f)
        //          // (iii) sj ≽ t for some j with 1 ≤ j ≤ m
        //          if (g.args.exists(compare(_, f) >= 0)) {
        //            log.info("%s has some args that are greater or equal then %s", g, f)
        //            -1
        //          } else {
        //            log.warning("Could not compare f : %s with g : %s", f, g)
        //            0
        //          }
        //        }
        case (Nary(f), Unary(g)) => {
          log.info("%s is N-Ary %s is UNAry", f, g)
          1
        }
        case (Unary(f), Nary(g)) => {
          log.info("%s is N-Ary %s is UNAry", g, f)
          -1
        }
        case (Unary(f), Unary(g)) => {
          log.info("%s is UNAry %s is UNAry", g, f)
          comparePrecedence(f, g)
        }
      }


    }

  }

  def comparePrecedence(a: FOLNode, b: FOLNode): Int = {
    // compare lexical
    val result = a.asInstanceOf[Term].name.compareTo(b.asInstanceOf[Term].name)
    log.info("Precedence comparison for %s and %s yielded %d", a, b, result)


    if (result > 0) {
      log.info("%s has higher precedence than %s", a, b)
    } else if (result < 0) {
      log.info("%s has higher precedence than %s", b, a)
    } else {
      log.info("%s has same precedence as %s", a, b)
    }

    result

  }


}