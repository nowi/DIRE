package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:47
 */

case class Function(name: String, terms: Term*) extends Term {
  override def toString = "%s(%s)" format (name, terms)
}