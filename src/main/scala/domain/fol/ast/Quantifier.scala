package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:23:55
 */

abstract class Quantifier extends Sentence {
  override def flatArgs: List[FOLNode] = {
    args match {
      case Some(args1) => args1.map({x: FOLNode => x.flatArgs}).flatten
    }
  }
}


case class UniversalQuantifer(filler: Sentence, variables: List[Variable]) extends Quantifier {
  val symbolicName = "forall"
  val args = Some(List(filler) ::: variables)

  override def toString = "∀ %s : %s" format (variables mkString ("", ",", ""), filler)
}
case class ExistentialQuantifer(filler: Sentence, variables: List[Variable]) extends Quantifier {
  val symbolicName = "exists"
  val args = Some(List(filler) ::: variables)

  override def toString = "∃ %s : %s" format (variables mkString ("", ",", ""), filler)
}