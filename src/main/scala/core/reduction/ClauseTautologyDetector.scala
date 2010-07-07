package de.unima.dire.core.reduction


import de.unima.dire.domain.fol.ast.{FOLNode, NegativeFOLLiteral, PositiveFOLLiteral}

import collection.mutable.{HashSet => MHashSet}
// import implicit converstion for FOLClausees

/**
 * User: nowi
 * Date: 24.04.2010
 * Time: 19:33:20
 */

trait ClauseTautologyDetection {
  def apply(clauseBuffer: Set[FOLNode]): Boolean
}




class ClauseTautologyDetector extends ClauseTautologyDetection {
  def apply(clauseBuffer: Set[FOLNode]): Boolean = {

    val positiveLits = new MHashSet[FOLNode]()
    val negativeLits = new MHashSet[FOLNode]()

    for (literal <- clauseBuffer.toList) {
      literal match {
        case PositiveFOLLiteral(node) => positiveLits += node
        case NegativeFOLLiteral(node) => negativeLits += node
      }


    }

    positiveLits.intersect(negativeLits)

    if (positiveLits.isEmpty) {
      false
    } else
      true

  }

}

