package domain.fol.ast


/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:20:57
 */

case class Predicate(name: String, terms: List[FOLNode]) extends Term {
  override val top = name
  override val args = terms

  override def arity = terms.size

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    Predicate(name, args.map({_.map(f)}))
  }

  override def flatArgs: List[FOLNode] = {
    args.map({x: FOLNode => x.flatArgs}).flatten
  }

  override def toString = "%s(%s)" format (name, terms mkString ("", ",", ""))


  // no nesting allowed !
  override def logicalEquals(obj: Any) = {
    obj match {
      case pred: Predicate => {
        assert(this match {
          case NestedPredicateLiteral(x) => false
          case _ => true
        }, "Cannot be nested")

        assert(obj match {
          case NestedPredicateLiteral(x) => false
          case _ => true
        }, "Cannot be nested")
        // only compare the non variable parts
        args.filter({!_.isInstanceOf[Variable]}) == pred.args.filter({!_.isInstanceOf[Variable]})
      }

      case _ => false
    }


  }

//  override val positions = {
//   val rootPos = Set(List(0))
//
//   val argsPos = for(index <- 0 until args.size)
//     yield args(index).positions match {
//       case index : Set[List[Int]] if(index.size == 1) => List(index)
//       case indexes : Set[List[Int]] => index :: indexes.toList
//
//     }
//
//
//   rootPos ++ argsPos
//
//  }
  
}


object Predicate {
  def apply(name: String, params: FOLNode*): Predicate = {
    Predicate(name, List(params: _*))
  }

}



