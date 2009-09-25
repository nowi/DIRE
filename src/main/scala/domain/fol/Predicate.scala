package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:57
 */

case class Predicate(name: String, terms: Term*) extends AtomicSentence with NamedObject {
  override def toString = "%s(%s)" format (name, terms)
}

