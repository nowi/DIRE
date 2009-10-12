package domain.fol.ast

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:54:46
 *
 * A standard clause == disjunction of literals
 */

case class Clause(literals: Set[FOLLiteral]) {
  override def toString = "Clause : %s" format (literals mkString ("[", "∨", "]"))


  lazy val positiveLiterals: Set[FOLLiteral] = {
    literals filter (_ match {
      case PositiveFOLLiteral(_) => true
      case _ => false

    })

  }
  lazy val negativeLiterals: Set[FOLLiteral] = {
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

