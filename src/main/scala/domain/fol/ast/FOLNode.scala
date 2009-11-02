package domain.fol.ast

/**
 * User: nowi
 * Date: 09.10.2009
 * Time: 15:02:25
 */

trait FOLNode {
  val args: List[FOLNode]
  val symbolicName: String

  // arity defaults to 0
  def arity = 0

  // this should maybe be implemented in each subclass
  def map(f: (FOLNode => FOLNode)): FOLNode

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