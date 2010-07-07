package de.unima.dire.core.unification


import de.unima.dire.domain.fol.ast.FOLNode
import de.unima.dire.domain.fol.functions.FOLAlgorithms
/**
 * User: nowi
 * Date: 08.04.2010
 * Time: 14:01:05
 */

class RobinsonUnificator {
  def apply(x: FOLNode, y: FOLNode) = {
    FOLAlgorithms.mgu(x, y)
  }


}