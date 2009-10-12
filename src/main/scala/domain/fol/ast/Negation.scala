package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:17:26
 */

case class Negation(filler: Sentence) extends Sentence {
  val symbolicName = "not"
  val args = Some(List(filler))

  override def flatArgs: List[FOLNode] = {
    args match {
      case Some(args1) => args1.map({x: FOLNode => x.flatArgs}).flatten
    }
  }

  override def toString = "¬(%s)" format (filler)
}