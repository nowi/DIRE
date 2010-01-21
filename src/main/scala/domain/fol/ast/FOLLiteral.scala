package domain.fol.ast

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:12:59
 */

object FOLLiteral {
  def unapply(node: FOLNode): Option[FOLNode] = {
    node match {
      case PositiveFOLLiteral(x) => Some(x)
      case NegativeFOLLiteral(x) => Some(x)
      case _ => None
    }
  }
}
// TODO something is wrong here , check this        
object NegativeFOLLiteral {
  def unapply(node: FOLNode): Option[FOLNode] = {
    node match {
      case Negation(PositiveFOLLiteral(literal)) => Some(literal)
      case _ => None
    }
  }
}

object PositiveFOLLiteral {
  def unapply(node: FOLNode): Option[FOLNode] = {
    node match {
      case x: AtomicSentence => Some(x)
      case _ => None
    }
  }
}