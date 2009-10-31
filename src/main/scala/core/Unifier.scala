package core

import domain.fol.ast._
import Substitutor._
import Standardizer._

/**
 * User: nowi
 * Date: 09.10.2009
 * Time: 14:59:57
 */

class Unifier {

  /**
   * <code>
   * function UNIFY(x, y, theta) returns a substitution to make x and y identical
   *   inputs: x, a variable, constant, list, or compound
   *           y, a variable, constant, list, or compound
   *
   * @return a Map<Variable, Term> representing the substitution (i.e. a set
   *         of variable/term pairs, see pg. 254 for a description) emmpty if failed to unify
   */
  def unify(x: FOLNode, y: FOLNode): Option[Map[Variable, FOLNode]] = {
    // standardize apart the terms
    val (xr, yr) = standardizeApart(x, y)
    unify(xr, yr, Some(Map[Variable, FOLNode]()))

  }

  private def unify(x: FOLNode, y: FOLNode, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    if (x == y) {
      theta
    } else {
      // if theta = failure then return failure
      theta match {
        case Some(t) => {
          (x, y) match {
            case (x: Variable, y: FOLNode) => unifyVar(x, y, theta)
            case (x: FOLNode, y: Variable) => unifyVar(y, x, theta)
            case (x: Constant, y: Constant) => None
            case (x: FOLNode, y: FOLNode) => {
              //IF COMPOUND?(x) and COMPOUND?(y) then
              //UNIFY(ARGS[x], ARGS[y], UNIFY(OP[x], OP[y], theta))
              unify(x.args, y.args, unify(x.symbolicName, y.symbolicName, theta));
            }
          }


        }
        case None => None

      }

    }


  }


  private def unify(xs: List[FOLNode], ys: List[FOLNode], theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    if (xs.size != ys.size) None
    theta match {
      case Some(t) => {
        (xs, ys) match {
          case (List(), List()) => theta
          case (x :: List(), y :: List()) => unify(x, y, theta)
          case (x :: xsss, y :: ysss) => unify(xsss, ysss, unify(x.asInstanceOf[FOLNode], y.asInstanceOf[FOLNode], theta))
        }

      }
      case None => None
    }

  }

  private def unify(x: String, y: String, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    theta match {
      case Some(t) => {
        if (x == y) theta
        else None
      }
      case None => None

    }

  }

  private def unifyVar(v: Variable, node: FOLNode, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    theta match {
      case Some(t) => {
        t.get(v) match {
          case Some(value) => unify(value, node, theta)
          // TODO something not right here , investigate
          //    else if (theta.keySet().contains(node)) unify(v, theta(node), theta)
          case None => {
            if (doesVarOccur(theta, v, node)) None
            else {
              cascadeSubstitution(theta, v, node);
            }
          }
        }
      }
      case None => None
    }

  }


  /**
   * Cascading substitutions

  Sometimes you get a substitution of the form σ =                      { z ← x, x ← a.
  Suppose you were to apply this substitution to p(z,x). The correct result is p(a,a).
  The reason is that you need to "cascade" the substitutions; if z takes the value x,
  you need to make sure that you haven't constrained x to be some other value.
  It would be incorrect to write p(x,a). This has particularly important consequences anytime
  you are trying to unify two expressions.
   */
  private def cascadeSubstitution(theta: Option[Map[Variable, FOLNode]], v: Variable, term: FOLNode): Option[Map[Variable, FOLNode]] = {
    // add the new subsitution
    theta match {
      case Some(t) => {
        val theta2: Map[Variable, FOLNode] = t + (v -> term)

        val theta3 = (for (key <- theta2.keySet) yield Map(key -> substitute(Some(theta2),
          theta2.get(key) match {
            case Some(value) => value
          })))

        Some(theta3.reduceLeft((map1, map2) => map1 ++ map2))


      }
      case None => None
    }

  }


  private def doesVarOccur(theta: Option[Map[Variable, FOLNode]], v: Variable, term: FOLNode): Boolean = {
    term match {
      case x: Function => x.vars contains v
      case _ => false
    }
  }


}


object Unifier {
  def unify(x: FOLNode, y: FOLNode): Option[Map[Variable, FOLNode]] = {
    val unifier = new Unifier
    unifier.unify(x, y)

  }

}