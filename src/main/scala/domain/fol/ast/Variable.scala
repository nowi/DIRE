package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:22:17
 */

case class Variable(name: String) extends Term {
  // is not complex, therefore no arguments
  override val symbolicName = name
  override val args = List(this)

  override def toString = "%s" format (name)

  override def flatArgs: List[FOLNode] = List(this)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    f(this)
  }

}