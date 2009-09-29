package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:57
 */

case class Predicate(name: String, terms: List[Term]) extends Term {
  override def toString = "%s(%s)" format (name, terms mkString ("", ",", ""))
}

