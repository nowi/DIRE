package domain.fol.ast

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:12:59
 */

abstract class FOLLiteral {
}


case class PositiveFOLLiteral(atom: AtomicSentence) extends FOLLiteral {
  override def toString = "%s" format (atom)
}
case class NegativeFOLLiteral(atom: AtomicSentence) extends FOLLiteral {
  override def toString = "-%s" format (atom)

}