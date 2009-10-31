package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:57
 */

case class Predicate(name: String, terms: List[FOLNode]) extends Term {
  val symbolicName = name
  val args = terms

  override def arity = terms.size

  override def flatArgs: List[FOLNode] = {
    args.map({x: FOLNode => x.flatArgs}).flatten
  }

  override def toString = "%s(%s)" format (name, terms mkString ("", ",", ""))
}

