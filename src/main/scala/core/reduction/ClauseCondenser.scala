package core.reduction


import collection.mutable.ListBuffer
import domain.fol.ast._
import domain.fol.functions.FOLAlgorithms
import FOLAlgorithms._
import helpers.HelperFunctions._
import helpers.Logging
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * Condensation Rule Every input clause as well as every derived clause is immerdiately
 * replaced by its condensation. Condensation corresponds to a special subcase of the subsumption rule :
 * The condensation of a clause C is a smallest factor of C subsuming C.
 * A short inspection of the completeness proof in [HR91] reveals that our modifications of ORP,
 * to be described in sections 5 and 6, do not affect its completeness when combined
 * with the condesation rule. ( compare also Rus91 )
 * User: nowi
 * Date: 24.04.2010
 * Time: 18:12:03
 */

trait ClauseCondensation {
  def apply(clauseBuffer: Set[FOLNode])(subsumptionChecker: Subsumption): Set[FOLNode]
}


object ClauseCondenser extends ClauseCondensation with Logging {
  // TODO , need efficient means of clause functionalities but on sequences of terms .. see ClauseBuffer in vampire

  override def apply(clauseBuffer: Set[FOLNode])(subsumptionChecker: Subsumption): Set[FOLNode] = {

    def getMatcher(a : FOLNode,b : FOLNode) = {
      // we can never match a positive with a negative literal
      (a, b) match {
        case (PositiveFOLLiteral(x), PositiveFOLLiteral(y)) => matcher(x, y)
        case (NegativeFOLLiteral(x), NegativeFOLLiteral(y)) => matcher(x, y)
        case _ => None
      }
    }


    // collect the candiate instantiations

    val candidate = (for (e1 <- clauseBuffer; e2 <- clauseBuffer; if (e1 != e2)) yield (e2, getMatcher(e1, e2))).find( {
      case (e2,Some(matcher)) => (subsumptionChecker((clauseBuffer - e2),clauseBuffer))
      case (e2,None) => false
    })

    candidate match {
      case Some((e2, Some(matcher))) => {
        // return inplace rewiritten and cut clause
        val condensed = (clauseBuffer - e2).map(_.rewrite(matcher))

        if(clauseBuffer.size - condensed.size > 0) {
          log.warning("%s condensed clause %s --> %s",this,clauseBuffer,condensed)
        }
        condensed

      }

      case _ => {
        // no condensing possible
        clauseBuffer
      }
    }


  }


}