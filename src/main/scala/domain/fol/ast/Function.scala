package domain.fol.ast


import org.slf4j.LoggerFactory

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:47
 */

case class Function(name: String, terms: List[FOLNode]) extends Term {
  val args = terms
  val symbolicName = name
  val log = LoggerFactory getLogger (this getClass)

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
    log.trace("FlatArgs for %s are : %s", this, flatArgs)
    flatArgs

  }

  override def toString = "%s(%s)" format (name, terms)


}


object Function {
  def apply(name: String, params: FOLNode*): Function = {
    Function(name, List(params: _*))
  }

}

