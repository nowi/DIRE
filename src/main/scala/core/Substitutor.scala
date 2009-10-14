package core

import VariableRewriter._

import domain.fol.ast._

/**
 * User: nowi
 * Date: 12.10.2009
 * Time: 15:00:39
 */

class Substitutor {

  /**
   * Note: Refer to Artificial Intelligence A Modern Approach (2nd Edition):
   * page 273.
   *
   * @param theta
   *            a substitution.
   * @param aSentence
   *            the substitution has been applied to.
   * @return a new Sentence representing the result of applying the
   *         substitution theta to aSentence.
   *
   */
  def substitute(theta: Option[Map[Variable, FOLNode]], node: FOLNode): FOLNode = {
    // use a variable rewriter
    theta match {
      case Some(map) => rewriteVars(node, map)
      case None => {
        println("No substition map theata specified , not rewriting term : %s" format (node))
        node
      }
    }
  }


}

object Substitutor {
  lazy val substitutor = new Substitutor

  def substitute(theta: Option[Map[Variable, FOLNode]], node: FOLNode): FOLNode = {
    substitutor.substitute(theta, node)

  }

}