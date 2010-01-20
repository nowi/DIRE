package core.ordering


import containers.ClauseStorage
import domain.fol.ast.FOLNode

/**
 * User: nowi
 * Date: 19.01.2010
 * Time: 18:38:10
 */

trait Precedence {
  lazy val symbolicNames = getSymbolicNames

  def getSymbolicNames: List[String]

  def apply(a: String) = symbolicNames.indexOf(a)

  def compare(a: String, b: String) = {
    // compare based on the position in the names list
    //this(a) compare this(b)
    apply(b) compare apply(a)
  }

  override def toString = "%s" format (symbolicNames mkString ("(", ">", ")"))

}

class LexicographicPrecedence(env: {
  val initialClauses: ClauseStorage}) extends Precedence {
  // build the precedence list from the linked initial  clausestore
  val initialClauses = env.initialClauses

  override def getSymbolicNames = sortedSymbolicNames(initialClauses)


  private def sortedSymbolicNames(clauses: ClauseStorage): List[String] = {
    val comparator = (x: String, y: String) => (x compareToIgnoreCase y) match {
      case result: Int if (result < 0) => true
      case result: Int if (result == 0) => false
      case result: Int if (result > 0) => false
    }
    val literals: Set[FOLNode] = clauses.values.flatMap({_.literals})
    val symbolicNames: Set[String] = literals.map({literal: FOLNode => literal.symbolicName})
    symbolicNames.toList.sort(comparator)

  }


}