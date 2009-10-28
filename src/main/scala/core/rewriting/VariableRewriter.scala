package core.rewriting

import domain.fol.ast._


/**
 * User: nowi
 * Date: 12.10.2009
 * Time: 19:03:48
 */
class VariableRewriter {

  /**Rewrite based on mapping theta
   * @param theta - the mapping
   * @returns rewritten folnode
   */
  def rewrite(node: FOLNode, theta: Map[Variable, FOLNode]): FOLNode = {
    // check all possible fol types

    // define the replacement function
    val f = (node: FOLNode, theta: Map[Variable, FOLNode]) => {
      node match {
        case x: Variable => theta.get(x) match {
          case Some(value) => value // we have a substition for this variable
          case None => x // there is no substitution return the original
        }
        case _ => node
      }
    }

    // apply this function partially , theta is fixed
    node.map(f(_: FOLNode, theta))


  }


}
object VariableRewriter {
  lazy val rewriter: VariableRewriter = new VariableRewriter

  def rewriteVars(node: FOLNode, theta: Map[Variable, FOLNode]): FOLNode = {
    val rewritten = rewriter.rewrite(node, theta)
    println("Rewritten %s --> %s (using Rewriter %s and theta : %s)" format (node, rewritten, this, theta))
    rewritten
  }

}