package core.rewriting


import domain.fol.ast._

/**
 * User: nowi                         ov
 * Date: 12.10.2009
 * Time: 15:00:39
 */

trait Substitution {
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
  def substitute(theta: Option[Map[Variable, FOLNode]], node: FOLNode): FOLNode


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
  def substitute(theta: Option[Map[Variable, FOLNode]], clause: FOLClause): FOLClause


}

// dependends on VariableRewriter
class Substitutor(env: {val variableRewriter: VariableRewriting}) extends Substitution {
  val log = net.lag.logging.Logger.get
  val variableRewriter = env.variableRewriter

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
      case Some(map) => variableRewriter.rewrite(node, map)
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
  def substitute(theta: Option[Map[Variable, FOLNode]], clause: FOLClause): FOLClause = {
    // use a variable rewriter
    theta match {
      case Some(map) => variableRewriter.rewriteClause(clause, map)
      case None => {
        log.warning("No substition map theata specified , not rewriting clause : %s" format (clause))
        clause
      }
    }
  }


}

