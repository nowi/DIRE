package de.unima.dire.core.matching

/**
 * User: nowi
 * Date: 15.04.2010
 * Time: 10:57:33
 */

import de.unima.dire.domain.fol.ast.FOLNode
import de.unima.dire.domain.fol.functions.FOLAlgorithms
import de.unima.dire.domain.fol.Substitution
/**
 * User: nowi
 * Date: 08.04.2010
 * Time: 14:01:05
 */

class Matcher {
  def matcher(x: FOLNode, y: FOLNode): Option[Substitution] = {
    FOLAlgorithms.matcher(x, y)
  }


}