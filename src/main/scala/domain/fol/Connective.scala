package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:23:46
 */


abstract class Connective extends Sentence

case class AndConnective(left: Sentence, right: Sentence) extends Connective {
  override def toString = "%s AND %s" format (left, right)
}
case class OrConnective(left: Sentence, right: Sentence) extends Connective {
  override def toString = "%s OR %s" format (left, right)
}
case class ImplicationConnective(left: Sentence, right: Sentence) extends Connective {
  override def toString = "%s => %s" format (left, right)
}
case class EqualityConnective(left: Sentence, right: Sentence) extends Connective {
  override def toString = "%s <=> %s" format (left, right)
}