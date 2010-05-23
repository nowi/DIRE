package core


import collection.jcl.MutableIterator.Wrapper
import collection.mutable.{Map => MMap}
import com.google.common.collect.{Multisets, HashMultiset, Multiset}
import domain.fol.ast._
import domain.fol.Substitution
import helpers.Logging
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
  def standardizeApart(x: FOLNode, y: FOLNode): (FOLNode, FOLNode,Substitution)


  /**
   * Make variablesnames unique within context of 2 clauses
   * @param c1 - clause
   * @param c2 - clause
   * @returns rewritten clauses pair
   */
  def standardizeApart(c1: Set[FOLNode], c2: Set[FOLNode]): (Set[FOLNode], Set[FOLNode],Substitution,Substitution,Substitution)

  def needStandardizing(x: FOLNode, y: FOLNode): Boolean

  def needStandardizing(c1: FOLClause, c2: FOLClause): Boolean
}


class Standardizer(env: {val variableRewriter: VariableRewriting}) extends Standardizing with Logging {
  val variableRewriter = env.variableRewriter
  lazy val randomizer: Random = new Random(System.currentTimeMillis)




  /**
   * Make variablesnames unique with in context of the 2 nodes
   * @param x - folnode
   * @param y - folnode
   * @returns Tupel of rewritten FOLNodes that have unique varnames with respect to each other
   */
  override def standardizeApart(x: FOLNode, y: FOLNode): (FOLNode, FOLNode,Substitution) = {

    // check terms need standardizing
    commonVars(x, y) match {
      case List() => { // empty list
        //record.trace("Tuple(%s,%s) needs no standardizing" format (x, y))
        (x, y,Map())
      }
      case cvs: List[Variable] => {
        // get the replacement map
        val renamed   = MMap[Variable,FOLNode]()
        val variableBag: Multiset[String] = new HashMultiset[String]()
        // create the substition transform
        val theta = cvs.map({old: Variable => (old -> renameVar(old,variableBag,renamed))}).foldLeft(Map[Variable, Variable]())(_ + _)
        // rewirte the smaller node , we default to x for now
        // TODO rewriteClause only the smaller node
        val xr = variableRewriter.rewrite(x, theta)
        //record.trace("Tuple(%s,%s) has been standardized apart to (%s,%s) by %s" format (x, y, xr, y, this))




        (xr, y,Substitution(Map() ++ renamed))
      }
    }

  }


  override def standardizeApart(c1: Set[FOLNode], c2: Set[FOLNode]) = {
    // check terms need standardizing
    commonVars(c1, c2) match {
      case List() => { // empty list
        //log.trace("clauses %s and %s need no standardizing", c1, c2)
        (c1, c2,Map(),Map(),Map())
      }
      case cvs: List[Variable] => {
         val variableBag: Multiset[String] = new HashMultiset[String]()
        val renamed   = MMap[Variable,FOLNode]()
        // create the substition transform
        val theta1 = cvs.map({old: Variable => (old -> renameVar(old,variableBag,renamed))}).foldLeft(Map[Variable, Variable]())(_ + _)
        val theta2 = cvs.map({old: Variable => (old -> renameVar(old,variableBag,renamed))}).foldLeft(Map[Variable, Variable]())(_ + _)
        // rewririte all nodes
        val cr1 = c1.map(_.rewrite(theta1))
        val cr2 = c2.map(_.rewrite(theta2))
        //log.trace("Clauses have been standardized apart and rewritten to %s by %s" format (cr1, cr2, this))

        (cr1, cr2,theta1,theta2,Substitution(Map() ++ renamed))
      }
    }
  }

  /**
   * Perfom renaming to random name
   */
  private def renameVar(x: Variable,variableBag : Multiset[String],renamingMap : scala.collection.mutable.Map[Variable,FOLNode]) : Variable= {
    // first add this variable to the bag
    variableBag.add(x.name)
    val newVarName = x.name + "_" + variableBag.count(x.name)
    // register in relacment map
    val newVariable = Variable(newVarName)
    renamingMap.put(newVariable,x.asInstanceOf[FOLNode])
    newVariable

  }


  def needStandardizing(x: FOLNode, y: FOLNode): Boolean = {
    commonVars(x, y).isEmpty
  }

  def needStandardizing(c1: FOLClause, c2: FOLClause): Boolean = {
    commonVars(c1, c2).isEmpty
  }


  private def commonVars(x: FOLNode, y: FOLNode): List[Variable] = {
    (x.flatArgs.filter({_.isInstanceOf[Variable]}) intersect y.flatArgs.filter({_.isInstanceOf[Variable]})).asInstanceOf[List[Variable]]
  }

  private def commonVars(nodes: Set[FOLNode]): List[Variable] = {
    // get all variables that are in common for any pair of nodes in the input nodes
    // we can ignore tuples with identical elements and reverse order to
    val cv = for (n1 <- nodes; n2 <- nodes) yield {commonVars(n1, n2)}
    cv.toList.flatten
  }

  private def commonVars(c1: Set[FOLNode], c2: Set[FOLNode]): List[Variable] = {
    val cv = (commonVars(c1) intersect commonVars(c2))
    cv.toList
  }
}
