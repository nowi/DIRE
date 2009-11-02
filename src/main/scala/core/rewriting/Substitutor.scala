package core.rewriting


import domain.fol.ast._

/**
 * User: nowi
 * Date: 12.10.2009
 * Time: 15:00:39
 */

trait Substitution extends VariableRewriting {
  override val log = net.lag.logging.Logger.get

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
      case Some(map) => rewrite(node, map)
      case None => {
        log.warning("No substition map theata specified , not rewriting term : %s" format (node))
        node
      }
    }
  }


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
  def substitute(theta: Option[Map[Variable, FOLNode]], clause: Clause): Clause = {
    // use a variable rewriter
    theta match {
      case Some(map) => rewriteClause(clause, map)
      case None => {
        log.warning("No substition map theata specified , not rewriting clause : %s" format (clause))
        clause
      }
    }
  }


}

class Substitutor extends Substitution {
}

