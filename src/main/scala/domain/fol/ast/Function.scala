package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:47
 */

case class Function(name: String, terms: List[FOLNode]) extends Term {
  val args = Some(terms)
  val symbolicName = name



  lazy val vars: List[Variable] =
  flatArgs.filter(_ match {
    case v: Variable => true
    case _ => false
  }).asInstanceOf[List[Variable]]


  def printVars {println(vars)}

  def printFlatArgs {println(flatArgs)}

  override def flatArgs: List[FOLNode] = {
    args match {
      case Some(args1) => args1.map({x: FOLNode => x.flatArgs}).flatten

    }
  }

  override def toString = "%s(%s)" format (name, terms)


}