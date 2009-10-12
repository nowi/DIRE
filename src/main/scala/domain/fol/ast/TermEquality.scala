package domain.fol.ast

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:05:22
 */

case class TermEquality(left: Term, right: Term) extends AtomicSentence {
  val symbolicName = "="
  val args: Option[List[FOLNode]] = Some(List(left, right))

  override def toString = "%s = %s" format (left, right)

  override def flatArgs: List[FOLNode] = {
    (left.args, right.args) match {
      case (Some(args1), Some(args2)) => (args1.map({x: FOLNode => x.flatArgs}) ::: args2.map({x: FOLNode => x.flatArgs})).flatten
      case (_, Some(args2)) => args2.map({x: FOLNode => x.flatArgs}).flatten
      case (Some(args1), _) => args1.map({x: FOLNode => x.flatArgs}).flatten
    }
  }


}