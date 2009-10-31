package domain.fol.ast

/**
 * User: nowi
 * Date: 09.10.2009
 * Time: 15:02:25
 */

trait FOLNode {
  val args: Option[List[FOLNode]]
  val symbolicName: String

  // arity defaults to 0
  def arity = 0

  // this should maybe be implemented in each subclass
  def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    this match {
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

  // get flattened args , empty lists if no args
  def flatArgs: List[FOLNode]

  def containsSubterm(subTerm: FOLNode): Boolean = {
    // for now check only identiy
    false
  }


}

object FOLNode {
  implicit def termToFOLNode(x: Term): FOLNode = x.asInstanceOf[FOLNode]

  implicit def sentenceToFOLNode(x: Term): FOLNode = x.asInstanceOf[FOLNode]
}

object Nary {
  def unapply(node: FOLNode): Option[FOLNode] = {
    if (node.arity > 0) Some(node)
    else None
  }
}

object Unary {
  def unapply(node: FOLNode): Option[FOLNode] = {
    if (node.arity == 0) Some(node)
    else None
  }
}