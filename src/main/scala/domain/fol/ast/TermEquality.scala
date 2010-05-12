package domain.fol.ast

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:05:22
 */

case class TermEquality(left: FOLNode, right: FOLNode) extends AtomicSentence {
  val top = "="
  val args: List[FOLNode] = List(left, right)

  override def toString = "%s = %s" format (left, right)

  override def flatArgs: List[FOLNode] = {
    (left.args.map({x: FOLNode => x.flatArgs}) ::: right.args.map({x: FOLNode => x.flatArgs})).flatten

  }

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    TermEquality(left.map(f), right.map(f))
  }


}