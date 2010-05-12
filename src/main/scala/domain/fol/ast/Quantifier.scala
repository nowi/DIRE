package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:23:55
 */

abstract class Quantifier extends Sentence {
  override def flatArgs: List[FOLNode] = {
    args.map({x: FOLNode => x.flatArgs}).flatten
  }
}


case class UniversalQuantifer(filler: Sentence, variables: List[Variable]) extends Quantifier {
  val top = "forall"
  val args = List(filler) ::: variables

  override def toString = "∀ %s : %s" format (variables mkString ("", ",", ""), filler)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    UniversalQuantifer(filler.map(f), args.map({_.map(f).asInstanceOf[Variable]}))
  }

}
case class ExistentialQuantifer(filler: Sentence, variables: List[Variable]) extends Quantifier {
  val top = "exists"
  val args = List(filler) ::: variables

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    ExistentialQuantifer(filler.map(f), args.map({_.map(f).asInstanceOf[Variable]}))
  }

  override def toString = "∃ %s : %s" format (variables mkString ("", ",", ""), filler)
}