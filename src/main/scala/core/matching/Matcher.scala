package core.matching

/**
 * User: nowi
 * Date: 15.04.2010
 * Time: 10:57:33
 */

import domain.fol.ast.{Variable, FOLClause, FOLNode}
import domain.fol.functions.FOLAlgorithms
import domain.fol.Substitution
import helpers.Logging
/**
 * User: nowi
 * Date: 08.04.2010
 * Time: 14:01:05
 */

class Matcher{
  def matcher(x: FOLNode, y: FOLNode) : Option[Substitution] = {
    FOLAlgorithms.matcher(x,y)
  }


}