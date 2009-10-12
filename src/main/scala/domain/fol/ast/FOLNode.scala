package domain.fol.ast

/**
 * User: nowi
 * Date: 09.10.2009
 * Time: 15:02:25
 */

trait FOLNode {
  val args: Option[List[FOLNode]]
  val symbolicName: String

  // get flattened args , empty lists if no args
  def flatArgs: List[FOLNode]


}

object FOLNode {
  implicit def termToFOLNode(x: Term): FOLNode = x.asInstanceOf[FOLNode]

  implicit def sentenceToFOLNode(x: Term): FOLNode = x.asInstanceOf[FOLNode]

}