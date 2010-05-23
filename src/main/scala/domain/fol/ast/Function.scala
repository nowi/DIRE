package domain.fol.ast

import collection.mutable.Map
import scala.collection.mutable.{Map => MMap}

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:47
 */

case class Function(name: String, terms: List[FOLNode]) extends Term {
  override lazy val args = terms
  override lazy val top = name

  override val arity = terms.size


  lazy val vars: List[Variable] =
  flatArgs.filter(_ match {
    case v: Variable => true
    case _ => false
  }).asInstanceOf[List[Variable]]





  def printVars {println(vars)}

  def printFlatArgs {println(flatArgs)}

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    Function(name, args.map({_.map(f)}))
  }


  override def flatArgs: List[FOLNode] = {
    val flatArgs: List[FOLNode] = args.map({x: FOLNode => x.flatArgs}).flatten
    flatArgs

  }


  override def shared = {
    // get the shared represenatations of this
    val sharedVer = this.map {_.shared}
    FOLNode.sharedNodes.getOrElseUpdate(sharedVer,sharedVer)
  }

  override def toString = "%s%s" format (name, terms mkString ("(", ",", ")"))


}


object Function {
  // override default apply method in order to implement caching
  def apply(name: String, params: FOLNode*) = {
    new Function(name, List(params: _*))
  }
}

object FunctionS {
  // override default apply method in order to implement caching
  def apply(name: String, params: FOLNode*) = {
    // create temp function
    val temp = new Function(name, List(params: _*))
    // return shared representation
    temp.shared
  }
}


