package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:17:26
 */

case class Negation(filler: Sentence) extends Sentence {
  override def toString = "¬(%s)" format (filler)
}