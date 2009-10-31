package domain.fol.ast


import net.lag.logging.Logger

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:47
 */

case class Function(name: String, terms: List[FOLNode]) extends Term {
  val args = Some(terms)
  val symbolicName = name
  val log: Logger = Logger.get

  override def arity = terms.size


  lazy val vars: List[Variable] =
  flatArgs.filter(_ match {
    case v: Variable => true
    case _ => false
  }).asInstanceOf[List[Variable]]


  def printVars {println(vars)}

  def printFlatArgs {println(flatArgs)}

  override def flatArgs: List[FOLNode] = {
    args match {
      case Some(args1) => {
        val flatArgs: List[FOLNode] = args1.map({x: FOLNode => x.flatArgs}).flatten
        log.info("FlatArgs for %s are : %s", this, flatArgs)
        flatArgs
      }

    }
  }

  override def toString = "%s(%s)" format (name, terms)


}
