package core


import domain.fol.ast._
import java.util.Random
import rewriting.VariableRewriter
import VariableRewriter._

/**
 * User: nowi
 * Date: 14.10.2009
 * Time: 14:16:46
 *
 * One way to handle this problem is to standardize apart the two sentences being unified,
 * which means renaming the variables of one (or both) to avoid name clashes.
 * Standardizer uses a VariableRewriter internally
 *
 * @see VariableRewriter
 *
 *
 */
class Standardizer {
  lazy val randomizer: Random = new Random(System.currentTimeMillis)

  /**
   * Make variablesnames unique with in context of the 2 nodes
   * @param x - folnode
   * @param y - folnode
   * @returns Tupel of rewritten FOLNodes that have unique varnames with respect to each other
   */
  def standardizeApart(x: FOLNode, y: FOLNode): (FOLNode, FOLNode) = {
    // check terms need standardizing
    commonVars(x, y) match {
      case List() => { // empty list
        println("Tuple(%s,%s) needs no standardizing" format (x, y))
        (x, y)
      }
      case cvs: List[Variable] => {
        // create the substition map
        val theta = cvs.map({old: Variable => (old -> renameVar(old))}).foldLeft(Map[Variable, Variable]())(_ + _)
        // rewirte the smaller node , we default to x for now
        // TODO rewrite only the smaller node
        val xr = rewriteVars(x, theta)
        println("Tuple(%s,%s) has been standardized apart to (%s,%s) by %s" format (x, y, xr, y, this))
        (xr, y)
      }
    }

  }


  /**
   * Perfom renaming to random name
   */
  private def renameVar(x: Variable): Variable = {
    Variable(x.name + randomizer.nextInt(1000))

  }


  def needStandardizing(x: FOLNode, y: FOLNode): Boolean = {
    commonVars(x, y).isEmpty
  }


  def commonVars(x: FOLNode, y: FOLNode): List[Variable] = {
    (x.flatArgs.filter({_.isInstanceOf[Variable]}) intersect y.flatArgs.filter({_.isInstanceOf[Variable]})).asInstanceOf[List[Variable]]
  }
}


object Standardizer {
  lazy val standardizer = new Standardizer

  def standardizeApart(x: FOLNode, y: FOLNode): (FOLNode, FOLNode) = {
    standardizer.standardizeApart(x, y)
  }

}