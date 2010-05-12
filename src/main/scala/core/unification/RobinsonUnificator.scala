package core.unification


import domain.fol.ast.{Variable, FOLClause, FOLNode}
import domain.fol.functions.FOLAlgorithms
import helpers.Logging
/**
 * User: nowi
 * Date: 08.04.2010
 * Time: 14:01:05
 */

class RobinsonUnificator {
  def apply(x: FOLNode, y: FOLNode) = {
    FOLAlgorithms.mgu(x,y)
  }


}