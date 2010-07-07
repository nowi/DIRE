package de.unima.dire.core.reduction


import de.unima.dire.domain.fol.ast._
import de.unima.dire.domain.fol.functions.FOLAlgorithms._
import de.unima.dire.helpers.Logging

import collection.mutable.ListBuffer
/**
 * User: nowi
 * Date: 25.04.2010
 * Time: 11:33:07
 */

class TrivialLiteralDeleter extends Logging {
  def apply(clauseBuffer: List[FOLNode]): List[FOLNode] = {

    def getMGU(a : FOLNode,b : FOLNode) = {
      // we can never match a positive with a negative literal
      (a, b) match {
        case (PositiveFOLLiteral(x), PositiveFOLLiteral(y))if(x.top == y.top) => mgu(x, y)
        case (NegativeFOLLiteral(x), NegativeFOLLiteral(y))if(x.top == y.top) => mgu(x, y)
        case _ => None
      }
    }

    // check if literals of this clauseBuffer can be unified beetween each other
    // each literal which can be unified with another literal is redundant
    val generalizations = new ListBuffer[FOLNode]()
    for (literal1 <- clauseBuffer;
         literal2 <- clauseBuffer;
         if ((literal1 != literal2) && !generalizations.contains(literal1)))
      {
        if (!getMGU(literal1, literal2).getOrElse(Map()).isEmpty) {
          generalizations += literal1 // literal1 is a generalization of one of the other literals
        }
      }


    // remove all literals that where more general from the original buffer
    val condensed = (clauseBuffer filterNot (generalizations.toList contains) )
    if (clauseBuffer.size - condensed.size > 0) {
      log.info("%s condensed clause %s --> %s", this, clauseBuffer, condensed)
    }
    condensed


  }


}