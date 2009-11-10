package domain.fol.ast


import org.slf4j.LoggerFactory

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:57
 */

case class Predicate(name: String, terms: List[FOLNode]) extends Term {
  val log = LoggerFactory getLogger (this getClass)
  val symbolicName = name
  val args = terms

  override def arity = terms.size

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    Predicate(name, args.map({_.map(f)}))
  }

  override def flatArgs: List[FOLNode] = {
    args.map({x: FOLNode => x.flatArgs}).flatten
  }

  override def toString = "%s(%s)" format (name, terms mkString ("", ",", ""))
}


object Predicate {
  def apply(name: String, params: FOLNode*): Predicate = {
    Predicate(name, List(params: _*))
  }

}



