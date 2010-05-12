package core.ordering


import scala.collection.mutable.{Map => MMap}
import containers.ClauseStorage._
import domain.fol.ast.FOLClause

/**
 * User: nowi
 * Date: 19.01.2010
 * Time: 18:38:10
 */

//trait Precedence {
//  lazy val symbolicNames = getSymbolicNames
//
//  def getSymbolicNames: List[String]
//
//  def apply(a: String) = symbolicNames.indexOf(a)
//
//  def compare(a: String, b: String) = {
//    // compare based on the position in the names list
//    //this(a) compare this(b)
//    apply(b) compare apply(a)
//  }
//
//  override def toString = "%s" format (symbolicNames mkString ("(", ">", ")"))
//
//}
//
//class LexicographicPrecedence(env: {
//  val initialClauses: List[FOLClause]}) extends Precedence {
//  // build the precedence list from the linked initial  clausestore
//  val initialClauses = env.initialClauses
//
//  override def getSymbolicNames = sortedSymbolicNames(initialClauses)
//
//
//  private def sortedSymbolicNames(clauses: List[FOLClause]): List[String] = {
//    val comparator = (x: String, y: String) => (x compareToIgnoreCase y) match {
//      case result: Int if (result < 0) => true
//      case result: Int if (result == 0) => false
//      case result: Int if (result > 0) => false
//    }
//    clauses.signature.toList.sort(comparator)
//
//  }
//
//
//}

trait Precedence {
  def compare(a: String, b: String) : Int

//  override def toString = "%s" format (symbolicNames mkString ("(", ">", ")"))

}

object LazyLexicographicPrecedence  extends Precedence {
  // build the precedence list from the linked initial  clausestore
  val cache : MMap[(String,String),Int] = scala.collection.mutable.HashMap[(String,String),Int]()
  
  private val comparator = (x: String, y: String) => (x compareToIgnoreCase y) match {
      case result: Int if (result < 0) => -1
      case result: Int if (result == 0) => 0
      case result: Int if (result > 0) => 1
    }

  

  override def compare(a: String, b: String) = {
    // check if we already compared this, if not add to cache
    val compared = comparator(a,b)
    cache.getOrElseUpdate((a,b),comparator(a,b))

  }
}