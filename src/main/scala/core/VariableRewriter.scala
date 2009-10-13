package core

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
  def rewriteVars(node: FOLNode, theta: Map[Variable, FOLNode]): FOLNode = {
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
    map(node, f(_: FOLNode, theta))

  }


  private def map(node: FOLNode, f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    node match {
      case x: Variable => f(x)
      case x: Constant => f(x)
      case x: Function => Function(x.name, x.terms.map({f(_)}))
      case x: Predicate => Predicate(x.name, x.terms.map({f(_)}))
      case x: AndConnective => AndConnective(f(x.left), f(x.right))
      case x: OrConnective => OrConnective(f(x.left), f(x.right))
      case x: UniversalQuantifer => UniversalQuantifer(f(x.filler), x.variables.map({f(_).asInstanceOf[Variable]}))
      case x: ExistentialQuantifer => ExistentialQuantifer(f(x.filler), x.variables.map({f(_).asInstanceOf[Variable]}))
      case x: Negation => Negation(f(x.filler))
      case x: TermEquality => TermEquality(f(x.left), f(x.right))


    }
  }
}
object VariableRewriter {
  lazy val rewriter: VariableRewriter = new VariableRewriter

  def rewriteVars(node: FOLNode, theta: Map[Variable, FOLNode]): FOLNode = {
    val rewritten = rewriter.rewriteVars(node, theta)
    println("Rewritten %s --> %s (using Rewriter %s and theta : %s)" format (node, rewritten, this, theta))
    rewritten
  }

}