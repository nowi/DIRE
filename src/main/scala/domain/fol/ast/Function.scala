package domain.fol.ast

import collection.mutable.Map
import scala.collection.mutable.{Map => MMap}

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:47
 */

case class Function(name: String, terms: List[FOLNode]) extends Term {

  override val args = terms
  override val top = name

  override def arity = terms.size


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




  override def toString = "%s%s" format (name, terms mkString ("(", ",", ")"))


  override def logicalEquals(obj: Any) = {
    obj match {
      case fun: Function => {
        assert(this match {
          case NestedFunctionLiteral(x) => false
          case _ => true
        }, "Cannot be nested")

        assert(obj match {
          case NestedFunctionLiteral(x) => false
          case _ => true
        }, "Cannot be nested")
        // only compare the non variable parts
        args.filter({!_.isInstanceOf[Variable]}) == fun.args.filter({!_.isInstanceOf[Variable]})
      }

      case _ => false
    }


  }


}


object Function {
  val cache = MMap[Function,Function]()
  // override default apply method in order to implement caching

  def shared(name: String, terms: List[FOLNode]): Function = {
    val f = new Function(name,terms)
    cache.getOrElseUpdate(f,f)
  }


  def apply(name: String, params: FOLNode*): Function = {
//    Function.shared(name, List(params: _*))
      Function(name, List(params: _*))
  }

}

