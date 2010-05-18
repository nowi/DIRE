package domain.fol

import scala.collection.mutable.{Map => MMap}
import ast.{FOLNode, Variable}
import collection.MapProxy

/**
 * User: nowi
 * Date: 01.04.2010
 * Time: 16:15:12
 */

case class Context(override val self:MMap[Variable,FOLNode]) extends MapProxy[Variable,FOLNode] {

  def bind(variable : Variable,term : FOLNode)  = {
    self.put(variable,term)
  }

  def binding(variable : Variable) : Option[FOLNode]= {
    get(variable)
  }

  // warning . apply means basically get binding
  override def apply(a: Variable) = {
    throw new IllegalStateException("WARNING .. didnt you really mean rewirte the term in this context")
  }

  def +(i: Tuple2[Variable, FOLNode]) = {
    bind(i._1,i._2)
  }
}

object Context {
  def apply() : Context = Context(MMap())
}