package domain.fol.ast

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:54:46
 *
 * A standard clause == disjunction of literals == Literal OR Literal ...
 */

case class Clause(literals: Set[FOLNode]) {
  // all folnodes have to be literals
  assert(literals forall ((_ match {
    case FOLLiteral(x) => true
    case _ => false
  })), "FOL nodes passed into a clause can only be Literals")


  def ++(that: Clause): Clause =
    Clause(literals ++ that.literals)


  def absoluteClause = Clause(absoluteLiterals)


  override def toString = "Clause : %s" format (literals mkString ("[", "∨", "]"))


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

object Clause {
  def apply(params: FOLNode*): Clause = {
    Clause(Set(params: _*))
  }

}

