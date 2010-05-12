package domain.fol.ast


/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:23:46
 */


abstract class Connective extends Sentence {
  override def flatArgs: List[FOLNode] = {
    (args.map({x: FOLNode => x.flatArgs})).flatten

  }


}

case class AndConnective(args: List[FOLNode]) extends Connective {
  //require (args.size > 1)
  val top = "and"

  def this(left: FOLNode, right: FOLNode) = this (List(left, right))


  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    AndConnective(args.map(f))
  }

  override def toString = "%s" format (args mkString ("(", "A", ")"))


  def ++(connective: AndConnective): AndConnective = {
    AndConnective(args ++ connective.args)
  }

  def --(connective: AndConnective): AndConnective = {
    AndConnective(args -- connective.args)
  }
}
case class OrConnective(args: List[FOLNode]) extends Connective {
  //require (args.size > 1)

  val top = "or"

  def this(left: FOLNode, right: FOLNode) = this (List(left, right))

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    OrConnective(args.map(f))
  }

  override def toString = "%s" format (args mkString ("(", "∨", ")"))


  def ++(connective: OrConnective): OrConnective = {
    OrConnective(args ++ connective.args)
  }

  def --(connective: OrConnective): OrConnective = {
    OrConnective(args -- connective.args)
  }

}


object OrConnective {
  def apply(left: FOLNode, right: FOLNode): OrConnective = OrConnective(List(left, right))
}

object AndConnective {
  def apply(left: FOLNode, right: FOLNode): AndConnective = AndConnective(List(left, right))
}




case class EqualityConnective(left: Sentence, right: Sentence) extends Connective {
  val top = "="
  val args = List(left, right)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    EqualityConnective(left.map(f), right.map(f))
  }

  override def toString = "%s ⇔ %s" format (left, right)
}



case class ImplicationConnective(left: Sentence, right: Sentence) extends Connective {
  val top = "->"
  val args = List(left, right)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    ImplicationConnective(left.map(f), right.map(f))
  }

  override def toString = "%s ⇒ %s" format (left, right)
}
