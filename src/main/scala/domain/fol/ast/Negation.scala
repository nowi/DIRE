package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:17:26
 */

case class Negation(filler: Sentence) extends Sentence {
  val symbolicName = "not"
  val args = List(filler)

  override def flatArgs: List[FOLNode] = {
    args.map({x: FOLNode => x.flatArgs}).flatten
  }


  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    Negation(filler.map(f))
  }

  override def toString = "¬(%s)" format (filler)
}