package domain.fol.ast

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:12:59
 */

object FOLLiteral {
  def unapply(node: FOLNode): Option[FOLNode] = {
    node match {
      case x: AtomicSentence => Some(x)
      case _ => None
    }
  }
}

object NegativeFOLLiteral {
  def unapply(node: FOLNode): Option[FOLNode] = {
    node match {
      case Negation(FOLLiteral(literal)) => Some(node)
      case _ => None
    }
  }
}

object PositiveFOLLiteral {
  def unapply(node: FOLNode): Option[FOLNode] = {
    node match {
      case FOLLiteral(x) => Some(x)
      case _ => None
    }
  }
}