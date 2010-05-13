package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:22:08
 */

case class Constant(name: String) extends Term {
  val args = List(this)
  val top = name

  override def toString = "%s" format (name)

  override def flatArgs: List[FOLNode] = List(this)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    f(this)
  }




}

object ConstantS {
  // override default apply method in order to implement caching
  def apply(name: String) = {
    // create temp object
    val temp = new Constant(name)
    // return shared representation
    temp.shared
  }
}
