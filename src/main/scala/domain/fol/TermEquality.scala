package domain.fol

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:05:22
 */

case class TermEquality(left: Term, right: Term) extends AtomicSentence {
  override def toString = "%s = %s" format (left, right)

}