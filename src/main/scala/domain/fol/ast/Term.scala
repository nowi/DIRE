package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:49:40
 */

trait Term extends AtomicSentence with NamedObject {
}


object Term {
  implicit def folNodeToTerm(x: FOLNode): Term = x.asInstanceOf[Term]


}

object Relation {
  def unapply(node: FOLNode): Option[FOLNode] = {
    node match {
      case x: Function => Some(x)
      case x: Predicate => Some(x)
      case _ => None
    }
  }
}