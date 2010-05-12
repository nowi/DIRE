package core.ordering

import helpers.Logging
import domain.fol.ast._


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
trait LiteralComparison {
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
class ALCLPOComparator(env: {
  val precedence: Precedence}) extends LiteralComparison with Logging {
  val precedence = env.precedence



  






  def compare(x: FOLNode, y: FOLNode) = {

    (x, y) match {
      case (Negation(a), Negation(b)) => compareLiterals(a, b)
      case (_, Negation(b)) if (x == b) => Some(1)
      case (Negation(a), _) if (y == a) => Some(-1)
      case (_, Negation(b)) => compareLiterals(x, b)
      case (Negation(a), _) => compareLiterals(a, y)
      case _ => compareLiterals(x, y)

    }


  }


  private def compareLiterals(a: FOLNode, b: FOLNode) = {
    //    Literals that contain different variables are incomparable
    assert((a, b) match {
      case (FOLLiteral(x), FOLLiteral(y)) => true
      case _ => false
    })



    // TODO more simplification based on ALCDR assumptions here !
    // 1.) Literals containing a function symbol precede literals that do not contain a function symbol.
    val aFuns = a.args.filter({_.isInstanceOf[Function]})
    val bFuns = b.args.filter({_.isInstanceOf[Function]})
    // first compare if any of the lits does not contain a function symbol
    (aFuns, bFuns) match {
    // 1.) Literals containing a function symbol precede literals that do not contain a function symbol.
      case (Nil, bfs :: bFuns) => Some(-1)
      case (afs :: aFuns, Nil) => Some(1)
      // 2.) Literals containing a function symbol are ordered according to the precedence of the function symbols.
      case (afs :: Nil, bfs :: Nil) => {
        // same functions as arguments in those predicates
        // compare based on precedence of the wrapping predicate
        Some(comparePrecedence(a, b))
      }
      // 3.) Literals not containing a function symbol are ordered according to the precedence of the predicate symbols.
      case (Nil, Nil) => {
        val aPreds = a.args.filter({_.isInstanceOf[Predicate]})
        val bPreds = b.args.filter({_.isInstanceOf[Predicate]})

        (aPreds, bPreds) match {
        // 1.) Literals containing a predicate symbol precede literals that do not contain a predicate symbol.
          case (Nil, bps :: bPreds) => Some(-1)
          case (aps :: aPreds, Nil) => Some(1)
          // 2.) Literals containing a predicate symbol are ordered according to the precedence of the predicate symbols.

          // 3.) Literals not containing a predaice must now be varisbles or constants , comparePrecedence
          case (Nil, Nil) => {
            Some(comparePrecedence(a, b))
          }
          case (aps :: aPreds, bps :: bPreds) => {
            // assert that there is only one predicate symbol per literal
            // --> no nesting
            assert(aPreds.size < 2 && bPreds.size < 2)

            Some(comparePrecedence(aps, bps))
          }

          case _ => {
            error("Match error")

          }

        }

      }

      case _ => {
        error("Match error")
      }


    }


  }


  def comparePrecedence(a: FOLNode, b: FOLNode): Int = {
    precedence.compare(a.top, b.top)
  }


}