package domain.fol.ast


/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:57
 */

case class Predicate(name: String, terms: List[FOLNode]) extends Term {
  override lazy val top = name
  override lazy val args = terms

  override val arity = terms.size

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    Predicate(name, args.map({_.map(f)}))
  }

  override def flatArgs: List[FOLNode] = {
    args.map({x: FOLNode => x.flatArgs}).flatten
  }

  override def toString = "%s(%s)" format (name, terms mkString ("", ",", ""))


  override def shared = {
    // get the shared represenatations of this
    val sharedVer = this.map {_.shared}
    FOLNode.sharedNodes.getOrElseUpdate(sharedVer,sharedVer)
  }
}



object Predicate {
  // override default apply method in order to implement caching
  def apply(name: String, params: FOLNode*) = {
    new Predicate(name, List(params: _*))
  }
}

object PredicateS {
  // override default apply method in order to implement caching
  def apply(name: String, params: FOLNode*) = {
    // create temp function
    val temp = new Predicate(name, List(params: _*))
    // return shared representation
    temp.shared
  }
}




