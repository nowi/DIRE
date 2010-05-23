package core.reduction


import collection.mutable.ListBuffer
import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast._
import domain.fol.functions.FOLAlgorithms
import domain.fol.Substitution
import helpers.Logging

import FOLAlgorithms._

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 15:19:43
 */

trait Reducing {
  def reduce(a: ClauseStorage): ClauseStorage
}

trait Subsumption {
  def apply(c: FOLClause, d: FOLClause): Boolean


}

object StillmannSubsumer extends Subsumption {
  def apply(c: FOLClause, d: FOLClause) = apply(c.literals,d.literals)

  def apply(a: Set[FOLNode], b: Set[FOLNode]): Boolean = {
    // intercept trivial sumption
    val c = a.toList.sort(_.top < _.top)
    val d = b.toList.sort(_.top < _.top)

//    val c = a.toList
//    val d = b.toList

    // TODO remove this
//    require(a.toList.sort(_.top < _.top) == c)
//    require(b.toList.sort(_.top < _.top) == d)


    def st(i: Int, j: Int, theta: Substitution): Boolean = {
      if (j < d.size) {
        var a = j
        val thetaLi = subs(theta, c(i))
        while (a < d.size && !doesUnify(thetaLi, d(a))) {
          a = a + 1
        }
        if (a < d.size) {
          val mgu = getMGU(thetaLi, d(a))
          if (i + 1 == c.size || st(i + 1, 1, theta ++ mgu.getOrElse(Map[Variable, FOLNode]()))) {
            true
          } else {
            st(i, a + 1, theta)
          }
        } else {
          false
        }

      } else {
        false
      }
    }

    def doesUnify(a: FOLNode, b: FOLNode): Boolean = {
      def checkSubstitution(a: FOLNode, b: FOLNode): Boolean = {
        getMGU(a, b) match {
          case Some(s: Substitution) => {
            true
          }
          case _ => false
        }
      }
      // take the order of the arugments into consideration

      // we can never unify a positive with a negative literal
      (a, b) match {
        case (PositiveFOLLiteral(x), PositiveFOLLiteral(y)) => checkSubstitution(x, y)
        case (NegativeFOLLiteral(x), NegativeFOLLiteral(y)) => checkSubstitution(x, y)
        case _ => false
      }


    }

    def getMGU(a: FOLNode, b: FOLNode) = {
      (a, b) match {
        case (PositiveFOLLiteral(x), PositiveFOLLiteral(y)) => matcher(x, y)
        case (NegativeFOLLiteral(x), NegativeFOLLiteral(y)) => matcher(x, y)
        case _ => error("we can never unify a positive with a negative literal")
      }
    }

    def subs(theta: Substitution, b: FOLNode): FOLNode = b.rewrite(theta)

    require(c.size > 0 && d.size > 0, "Argument clauses to Stillman algorithm cannot be empty")
    //           [¬(O2Proceedings(U))V¬(O2Review(U))]
    


    if ((a eq b ) || (a == b)) {
      true
    } else {
      //    Let C = (L,, . . . , L,) and D = (Ki, . . . , Km).
      // init the map
      val emptyTheta = domain.fol.Substitution()
      val result = st(0, 0, emptyTheta)
      result
    }

  }
}

