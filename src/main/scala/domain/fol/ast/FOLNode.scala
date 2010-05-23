package domain.fol.ast

import core.ordering.LiteralComparison
import domain.fol.functions.FOLAlgorithms._

import functions.FOLAlgorithms
import sun.reflect.generics.reflectiveObjects.NotImplementedException
        import scala.collection.mutable.{Map => MMap}
/**
 * User: nowi
 * Date: 09.10.2009
 * Time: 15:02:25
 */

trait FOLNode {
  val args: List[FOLNode]
  // default no bindings
  //val bindings : Context = Context()

  val top: String

  // subclasses should override this for corret sharing of arguments
  def shared : FOLNode  = {
    FOLNode.sharedNodes.getOrElseUpdate(this ,this)
  }

  // arity defaults to 0
  val arity : Int = 0

  // TODO FIX THIS
  lazy val positive = {
    this match {
      case Negation(filler) => false
      case _ => {
        true
        // should not be here
      }
    }
  }

  lazy val negative = !this.positive



  // this should maybe be implemented in each subclass
  // take care when mapping over shared terms , maping could return unshared structures
  def map(f: (FOLNode => FOLNode)): FOLNode

  // get flattened args , empty lists if no args
  def flatArgs: List[FOLNode]

//  val positions: List[List[Any]] = {
//    this match {
//      case Complex(name, args) => {
//        val is = for (index <- 0 until args.size)
//        yield (args(index).positions match {
//            case List(List(0)) => {
//              // single element sub positions
//              // extract the single element and concat
//              val i: List[Int] = List(index + 1)
//              i
//
//            }
//            case indexes: List[List[Int]] => {
//              // multiple sub index paths
//              // extract them all , concat each one with the current index path
//              //val i : List[Int] = indexes.map({indexPath: List[Int] => List(index+1) ::: indexPath}).toList.flatten
//              val i: List[List[Int]] = indexes.map({
//                indexPath: List[Int] => indexPath match {
//                  case List(0) => List(index + 1)
//                  case _ => List(index + 1) ::: indexPath
//                }
//              })
//              i
//            }
//
//          })
//
//
//        (List(List(0)) ::: is.toList)
//      }
//
//      case _ => {
//        List(List(0))
//      }
//
//    }
//
//
//  }

  //positions access
  def apply(indexPath: List[Any]): FOLNode = {
    //      if (!positions.contains(indexPath)) {
    //        throw new IndexOutOfBoundsException("The specified indexpath is not valid")
    //
    //      }
    // return the subterm at the specifed indexpath
    indexPath match {
      case List(0) => this
      case List(List(index)) => this(List(index))
      case List(indexes : List[Any]) => this(indexes)
      case List(index) :: List(indexes) => {
        args(index.asInstanceOf[Int] - 1).apply(List(indexes))
      }
      case (index:Int) :: List(indexes) => {
        args(index.asInstanceOf[Int] - 1).apply(List(indexes))
      }
      case (index:Int) :: Nil => {
        args(index.asInstanceOf[Int] - 1)
      }
    }
  }

  //rewrite with substitution
  def apply(substitution: Substitution): FOLNode = {
    rewrite(substitution)
  }


  /**Rewrite based on mapping theta
   * @param theta - the mapping
   * @returns rewritten folnode
   */
  def rewrite(s: Substitution): FOLNode = {
    if(s.isEmpty) {
      this
    } else {
      // check all possible fol types
      // define the replacement function
      val f = (node: FOLNode, s: Substitution) => {
        node match {
          case x: Variable => {
            // check if there is a substitution
            s.getOrElse(x, node)
          }
          case _ => node
        }
      }
      // apply this partial function to this node with fixed substitution s
      val rewritten = map(f(_: FOLNode, s))
      rewritten
    }

  }




  def normalize = {
    // crate the first occurrence map
    val uniqueVars  = ( Set[Variable]() /: flatArgs.filter(_ match {
      case Variable(name) => true
      case _ => false
    }).asInstanceOf[List[Variable]] )(_ + _)
    // map each unique variable to a indicator variable --> substitution
    val sub  = uniqueVars.toList zip IndicatorVariable.rangeTo(uniqueVars.size)
    // apply this substitution to this term
    this.rewrite(sub)
  }





  def containsSubterm(subTerm: FOLNode): Boolean = {
    // for now check only identiy
    false
  }




  def negate() : FOLNode = {
    this match {
      case node if(positive) => Negation(node)
      case Negation(filler) => filler
    }
  }


 














  def logicalEquals(obj: Any) = equals(obj)
}

object FOLNode {

  val sharedNodes : MMap[FOLNode,FOLNode]= MMap()

  implicit def termToFOLNode(x: Term): FOLNode = x.asInstanceOf[FOLNode]

  implicit def sentenceToFOLNode(x: Term): FOLNode = x.asInstanceOf[FOLNode]
}

// extractor for complex fol terms
object Complex {
  def unapply(node: FOLNode): Option[(String, List[FOLNode])] = {
    if (node.arity > 0) Some((node.top, node.args))
    else None
  }
}

object Node {
  def unapply(node: FOLNode): Option[(String, List[FOLNode])] = {
    Some((node.top, node.args))
  }
}



object Nary {
  def unapply(node: FOLNode): Option[FOLNode] = {
    if (node.arity > 0) Some(node)
    else None
  }
}

object Unary {
  def unapply(node: FOLNode): Option[FOLNode] = {
    if (node.arity == 0) Some(node)
    else None
  }
}