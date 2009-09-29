package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:23:55
 */

abstract class Quantifier extends Sentence


case class UniversalQuantifer(filler: Sentence, variables: List[Variable]) extends Quantifier {
  override def toString = "∀ %s %s" format (variables mkString ("", ",", ""), filler)
}
case class ExistentialQuantifer(filler: Sentence, variables: List[Variable]) extends Quantifier {
  override def toString = "∃ %s %s" format (variables mkString ("", ",", ""), filler)
}