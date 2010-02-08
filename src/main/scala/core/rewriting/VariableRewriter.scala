package core.rewriting

import domain.fol.ast._

  import helpers.Logging

/**
 * User: nowi
 * Date: 12.10.2009
 * Time: 19:03:48
 */
trait VariableRewriting {
  /**Rewrite based on mapping theta
   * @param theta - the mapping
   * @returns rewritten folnode
   */
  def rewrite(node: FOLNode, theta: Map[Variable, FOLNode]): FOLNode

  /**Rewrite based on mapping theta
   * @param theta - the mapping
   * @returns rewritten clause
   */
  def rewriteClause(clause: FOLClause, theta: Map[Variable, FOLNode]): FOLClause
}

class VariableRewriter extends VariableRewriting with Logging {

  /**Rewrite based on mapping theta
   * @param theta - the mapping
   * @returns rewritten folnode
   */
  def rewrite(node: FOLNode, theta: Map[Variable, FOLNode]): FOLNode = {
    // check all possible fol types

    // define the replacement function
    val f = (node: FOLNode, theta: Map[Variable, FOLNode]) => {
      node match {
        case x: Variable => {
          // check if there is a substitution
          theta.getOrElse(x, node)
        }
        case _ => node
      }
    }

    // apply this function partially , theta is fixed
    node.map(f(_: FOLNode, theta))


  }


  /**Rewrite based on mapping theta
   * @param theta - the mapping
   * @returns rewritten clause
   */
  def rewriteClause(clause: FOLClause, theta: Map[Variable, FOLNode]): FOLClause = {
    // check all possible fol types
    log.trace("Rewriting Clause %s", clause)

    // define the replacement function
    val f = (node: FOLNode, theta: Map[Variable, FOLNode]) => {
      node match {
        case x: Variable => {
          // check if there is a substitution
          theta.getOrElse(x, node)
        }
        case _ => node
      }
    }

    val rewrittenClauses = clause.literals.map({x: FOLNode => x.map(f(_: FOLNode, theta))})
    // apply this function partially , theta is fixed
    StandardClause(rewrittenClauses)

  }

}