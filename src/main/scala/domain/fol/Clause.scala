package domain.fol

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:54:46
 *
 * A disjunction of literals
 */

case class Clause(literals: Set[FOLLiteral]) {
  override def toString = "Clause : %s" format (literals mkString ("[", ",", "]"))

  def positiveLiterals: Set[FOLLiteral] = {
    literals filter (_ match {
      case PositiveFOLLiteral(_) => true
      case _ => false

    })

  }

  def negativeLiterals: Set[FOLLiteral] = {
    literals filter (_ match {
      case NegativeFOLLiteral(_) => true
      case _ => false

    })

  }


}