package core.rewriting


import domain.fol.ast._
import org.slf4j.LoggerFactory

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


  def reverseSubstitute(theta: Option[Map[Variable, FOLNode]], node: FOLNode): FOLNode


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

  def reverseSubstitute(theta: Option[Map[Variable, FOLNode]], clause: FOLClause): FOLClause


}

// dependends on VariableRewriter
class Substitutor(env: {val variableRewriter: VariableRewriting}) extends Substitution {
  val log = LoggerFactory getLogger (this getClass)
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
        //        log.trace("No substition map theata specified , not rewriting term : {}" node)
        node
      }
    }
  }


  def reverseSubstitute(theta: Option[Map[Variable, FOLNode]], node: FOLNode) = {
    // use a variable rewriter
    theta match {
      case Some(map) => {
        // we need to reverse the map
        val reversedMap = (for ((key, value) <- map) yield Map(value -> key)).reduceLeft(_ ++ _)

        variableRewriter.rewrite2(node, reversedMap)
      }
      case None => {
        //        log.trace("No substition map theata specified , not rewriting term : {}" node)
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
        //        log.trace("No substition map theata specified , not rewriting clause : {}" clause)
        clause
      }
    }
  }


  def reverseSubstitute(theta: Option[Map[Variable, FOLNode]], clause: FOLClause) = {
    theta match {
      case Some(map) => {
        // we need to reverse the map
        val reversedMap = (for ((key, value) <- map) yield Map(value -> key)).reduceLeft(_ ++ _)
        variableRewriter.rewriteClause2(clause, reversedMap)
      }
      case None => {
        //        log.trace("No substition map theata specified , not rewriting clause : {}" clause)
        clause
      }
    }

  }
}

