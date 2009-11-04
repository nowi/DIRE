package core


import com.google.common.collect.{HashMultiset, Multiset}
import domain.fol.ast._
import java.util.Random
import rewriting.VariableRewriting

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
trait Standardizing {
  /**
   * Make variablesnames unique with in context of the 2 nodes
   * @param x - folnode
   * @param y - folnode
   * @returns Tupel of rewritten FOLNodes that have unique varnames with respect to each other
   */
  def standardizeApart(x: FOLNode, y: FOLNode): (FOLNode, FOLNode)


  /**
   * Make variablesnames unique within context of 2 clauses
   * @param c1 - clause
   * @param c2 - clause
   * @returns rewritten clauses pair
   */
  def standardizeApart(c1: Clause, c2: Clause): (Clause, Clause)

  def needStandardizing(x: FOLNode, y: FOLNode): Boolean

  def needStandardizing(c1: Clause, c2: Clause): Boolean
}


class Standardizer(env: {val variableRewriter: VariableRewriting}) extends Standardizing {
  val variableRewriter = env.variableRewriter
  lazy val randomizer: Random = new Random(System.currentTimeMillis)

  lazy val variableBag: Multiset[String] = HashMultiset.create()

  val log = net.lag.logging.Logger.get

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
        log.info("Tuple(%s,%s) needs no standardizing" format (x, y))
        (x, y)
      }
      case cvs: List[Variable] => {
        // create the substition transform
        val theta = cvs.map({old: Variable => (old -> renameVar(old))}).foldLeft(Map[Variable, Variable]())(_ + _)
        // rewirte the smaller node , we default to x for now
        // TODO rewriteClause only the smaller node
        val xr = variableRewriter.rewrite(x, theta)
        log.info("Tuple(%s,%s) has been standardized apart to (%s,%s) by %s" format (x, y, xr, y, this))
        (xr, y)
      }
    }

  }


  def standardizeApart(c1: Clause, c2: Clause): (Clause, Clause) = {
    // check terms need standardizing
    commonVars(c1, c2) match {
      case List() => { // empty list
        log.info("clauses %s and %s need no standardizing", c1, c2)
        (c1, c2)
      }
      case cvs: List[Variable] => {
        // create the substition transform
        val theta1 = cvs.map({old: Variable => (old -> renameVar(old))}).foldLeft(Map[Variable, Variable]())(_ + _)
        val theta2 = cvs.map({old: Variable => (old -> renameVar(old))}).foldLeft(Map[Variable, Variable]())(_ + _)
        // rewririte all nodes
        val cr1 = variableRewriter.rewriteClause(c1, theta1)
        val cr2 = variableRewriter.rewriteClause(c2, theta2)
        log.trace("Clauses have been standardized apart and rewritten to %s by %s" format (cr1, cr2, this))

        (cr1, cr2)
      }
    }
  }

  /**
   * Perfom renaming to random name
   */
  private def renameVar(x: Variable): Variable = {
    // first add this variable to the bag
    variableBag.add(x.name)
    val newVarName = x.name + "_" + variableBag.count(x.name)
    Variable(newVarName)

  }


  def needStandardizing(x: FOLNode, y: FOLNode): Boolean = {
    commonVars(x, y).isEmpty
  }

  def needStandardizing(c1: Clause, c2: Clause): Boolean = {
    commonVars(c1, c2).isEmpty
  }


  private def commonVars(x: FOLNode, y: FOLNode): List[Variable] = {
    (x.flatArgs.filter({_.isInstanceOf[Variable]}) intersect y.flatArgs.filter({_.isInstanceOf[Variable]})).asInstanceOf[List[Variable]]
  }

  private def commonVars(nodes: List[FOLNode]): List[Variable] = {
    // get all variables that are in common for any pair of nodes in the input nodes
    // we can ignore tuples with identical elements and reverse order to
    val cv = for (n1 <- nodes; n2 <- nodes) yield {commonVars(n1, n2)}
    log.info("Common vars for node List : %s are %s", nodes, cv)
    cv.flatten
  }

  private def commonVars(c1: Clause, c2: Clause): List[Variable] = {
    val cv = (commonVars(c1.literals.toList) intersect commonVars(c2.literals.toList))
    log.info("Common vars for clause : %s and  clause : %s are %s", c1, c2, cv)
    cv
  }


}