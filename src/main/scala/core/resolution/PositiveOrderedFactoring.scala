package core.resolution


import collection.mutable.ListBuffer
import domain.fol.ast._
import domain.fol.functions.FOLAlgorithms._
import domain.fol.Substitution
import ordering.LiteralComparison
import selection.LiteralSelection

/**
 * User: nowi
 * Date: 28.04.2010
 * Time: 15:49:22
 */


trait Factoring extends Inference {
  def apply(clause1: FOLClause): InferenceResult

}

trait FactoringResult extends InferenceResult

//case class SuccessfullPositiveFactoring(override val result : List[FOLNode],val parent:FOLClause) extends FactoringResult
//
//case class FailedPositiveFactoring(val parent:FOLClause) extends FactoringResult {
//  override val result = Nil
//}


trait PositiveFactoring {
  def apply(clause: List[FOLNode]): List[FOLNode]
}

object PositiveFactorer extends PositiveFactoring {
  override def apply(clause: List[FOLNode]): List[FOLNode] = {

    def getMGU(a: FOLNode, b: FOLNode) = {
      // we can never match a positive with a negative literal
      (a, b) match {
        case (PositiveFOLLiteral(x), PositiveFOLLiteral(y)) => mgu(x, y)
        case (NegativeFOLLiteral(x), NegativeFOLLiteral(y)) => mgu(x, y)
        case _ => None
      }
    }

    if (clause.size < 2) {
      log.debug("%s NOT Facoring UNIT or EMPTY clause %s", this, clause)
      clause
    } else {

      // first clear duplicate literals
      val buffer = new ListBuffer[FOLNode]() ++ clause.removeDuplicates

      // collect the candiate instantiations
      val candidate = (for (e1 <- clause; e2 <- clause if (e1 != e2)) yield (e2, getMGU(e1, e2))).find({
        case (e2, Some(matcher)) => true // foudn some unifier , might be even the trivial one
        case (e2, None) => false
      })

      candidate match {
        case Some((e2, Some(matcher))) => {
          // return inplace rewiritten and cut clause
          buffer -= e2
          val factoredClause = buffer.map(_.rewrite(matcher)).toList
          if (clause.size - factoredClause.size > 0) {
            log.warning("%s Facoring clause %s --> %s", this, clause, factoredClause)
          } else {
            error("Should not factor to empty clause")
          }
          factoredClause

        }

        case _ => {
          // no condensing possible
          clause
        }
      }

    }


  }


}






class PositiveOrderedFactoring(env: {val selector: LiteralSelection; val literalComparator: LiteralComparison}) extends PositiveFactoring with helpers.Logging {
  implicit def listFOLnode2ALCDClause(list: List[FOLNode]): FOLClause = ALCDClause(list)

  implicit val literalSelector = env.selector
  implicit val literalComparator = env.literalComparator


  override def apply(clause: List[FOLNode]): List[FOLNode] = {

    // Aσ is maximal with respect to Cσ ∨ Bσ
    def condition2(b: FOLNode,matcher : Substitution) = {
      (clause - b).rewrite(matcher).contains(b.rewrite(matcher))
    }

    // 3. nothing is selected in Cσ ∨ Aσ ∨ Bσ
    def condition3(matcher : Substitution) = clause.rewrite(matcher).selectedLits.isEmpty


    def getMGU(a: FOLNode, b: FOLNode) = {
      // we can never match a positive with a negative literal
      (a, b) match {
        case (PositiveFOLLiteral(x), PositiveFOLLiteral(y)) => mgu(x, y)
        case (NegativeFOLLiteral(x), NegativeFOLLiteral(y)) => mgu(x, y)
        case _ => None
      }
    }

    if (clause.size < 2) {
      log.debug("%s NOT Facoring UNIT or EMPTY clause %s", this, clause)
      clause
    } else {

      // first clear duplicate literals
      val buffer = new ListBuffer[FOLNode]() ++ clause.removeDuplicates

      // collect the candiate instantiations

      val candidate = (for (e1 <- clause; e2 <- clause if (e1 != e2)) yield (e2, getMGU(e1, e2))).find({
        case (e2, Some(matcher)) => {
          // foudn some unifier , might be even the trivial one
          // check for ordering constraints
          condition2(e2,matcher) && condition3(matcher)
        }
        case (e2, None) => false
      })

      candidate match {
        case Some((e2, Some(matcher))) => {
          // return inplace rewiritten and cut clause
          buffer -= e2
          val factoredClause = buffer.map(_.rewrite(matcher)).toList
          if (clause.size - factoredClause.size > 0) {
            log.warning("%s Facoring clause %s --> %s", this, clause, factoredClause)
          } else {
            error("Should not factor to empty clause")
          }
          factoredClause

        }

        case _ => {
          // no condensing possible
          clause
        }
      }

    }


  }
}