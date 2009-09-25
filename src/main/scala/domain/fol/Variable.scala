package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:22:17
 */

case class Variable(name: String) extends Term {
  override def toString = "%s" format (name)
}