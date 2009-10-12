package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:22:08
 */

case class Constant(name: String) extends Term {
  val args = None
  val symbolicName = name

  override def toString = "%s" format (name)

  override def flatArgs: List[FOLNode] = {
    args match {
      case None => List(this)
    }
  }

}