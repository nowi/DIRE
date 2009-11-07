package domain.fol.ast

trait FOLClause {
  // all folnodes have to be literals
  val literals: Set[FOLNode]


  def ++(that: FOLClause): FOLClause


  def --(that: FOLClause): FOLClause


  def -(that: FOLNode): FOLClause

  def +(that: FOLNode): FOLClause

  def absoluteClause: FOLClause


  lazy val absoluteLiterals: Set[FOLNode] = {
    literals map (_ match {
      case Negation(filler) => filler
      case x: FOLNode => x

    })

  }

  lazy val positiveLiterals: Set[FOLNode] = {
    literals filter (_ match {
      case PositiveFOLLiteral(_) => true
      case _ => false

    })

  }
  lazy val negativeLiterals: Set[FOLNode] = {
    literals filter (_ match {
      case NegativeFOLLiteral(_) => true
      case _ => false

    })

  }

  lazy val isEmpty = literals.isEmpty

  lazy val isUnit = literals.size == 1

  // A Horn clause is a disjunction of literals of which at most one is
  // positive.
  lazy val isHorn = !isEmpty && positiveLiterals.size <= 1

  lazy val isDefinitive = !isEmpty && positiveLiterals.size == 1

}

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:54:46
 *
 * A standard clause == disjunction of literals == Literal OR Literal ...
 */
case class Clause(literals: Set[FOLNode]) extends FOLClause {
  // all folnodes have to be literals
  assert(literals forall ((_ match {
    case FOLLiteral(x) => true
    case _ => false
  })), "FOL nodes passed into a clause can only be Literals")


  override def ++(that: FOLClause): FOLClause =
    Clause(literals ++ that.literals)


  override def --(that: FOLClause): FOLClause =
    Clause(literals -- that.literals)


  override def -(that: FOLNode): FOLClause =
    Clause(literals - that)


  override def +(that: FOLNode): FOLClause =
    Clause(literals + that)


  override def absoluteClause = Clause(absoluteLiterals)


  override def toString = "Clause : %s" format (literals mkString ("[", "∨", "]"))


}

case class EmptyClause extends FOLClause {
  override val literals = Set[FOLNode]()

  override def toString = "■"

  override lazy val isEmpty = true


  override def absoluteClause = EmptyClause()

  override def +(that: FOLNode) = EmptyClause()

  override def -(that: FOLNode) = EmptyClause()

  override def ++(that: FOLClause) = EmptyClause()

  override def --(that: FOLClause) = EmptyClause()
}

object Clause {
  def apply(params: FOLNode*): Clause = {
    Clause(Set(params: _*))
  }

}


