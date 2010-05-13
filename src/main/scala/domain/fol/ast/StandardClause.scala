package domain.fol.ast

import collection.mutable.ListBuffer
import core.selection.LiteralSelection
import parsers.SPASSIntermediateFormatParser._

import core.ordering.LiteralComparison
import core.resolution.UniqueLiteralResolution

trait FOLClause {
  val literals : List[FOLNode]

  def uniqueResolvableLit(implicit resolver: (FOLClause => Option[FOLNode])): Option[FOLNode] = {
    val urlit = resolver(this)
    urlit
  }

  def selectedLits(implicit selector: LiteralSelection): List[FOLNode] = {
    selector.selectedLiterals(this)
  }

  def maxLits(implicit comperator: LiteralComparison): List[FOLNode] = {
    require(!isEmpty, "There cannot be a max Lit in Empty Clause")

    // we have no max for this comparator , determine the max and cache it
    var maximumLiterals: List[FOLNode] = List(literals.head)
    val iter = this.literals.elements
    while (iter.hasNext) {
      val lit = iter.next
      val max = maximumLiterals.head
      if (lit != max) {
        comperator.compare(lit, maximumLiterals.head) match {
          case Some(1) => maximumLiterals = List(lit) // found a greater lit , this is new maxLit
          case Some(0) => maximumLiterals = lit :: maximumLiterals // found same as current max , add to maxlits
          case Some(-1) => None
          case None => {
            None
          }
        }

      }

    }

    maximumLiterals

  }

  lazy val isUnit = size == 1

  // A Horn clause is a disjunction of literals of which at most one is
  // positive.
  lazy val isHorn = !isEmpty && positiveLiterals.size <= 1

  lazy val isDefinitive = !isEmpty && positiveLiterals.size == 1


  lazy val isEmpty = literals.isEmpty

  lazy val size = literals.size


    





//  // unique resolvable literal cache
//  protected var uniqueLits: Map[UniqueLiteralResolution, Option[FOLNode]] = Map[UniqueLiteralResolution, Option[FOLNode]]()

  lazy val absoluteLiterals: List[FOLNode] = {
    literals map (_ match {
      case Negation(filler) => filler
      case x: FOLNode => x

    })

  }

  lazy val positiveLiterals: List[FOLNode] = {
    literals filter (_ match {
      case PositiveFOLLiteral(_) => true
      case _ => false

    })

  }
  lazy val negativeLiterals: List[FOLNode] = {
    (literals filter (_ match {
      case NegativeFOLLiteral(_) => true
      case _ => false

    }))

  }


  lazy val signature = {
    literals.map({
      x: FOLNode => x match {
        case PositiveFOLLiteral(literal) => (literal.top, literal.arity)
        case NegativeFOLLiteral(literal) => ("-" + literal.top, literal.arity)
      }

    })

  }


  /**Retrieve the literals which is associated with the given literal signature.
   * @param litSig the signature of the literal (symblicname,arity)
   * @return the literals
   */
  def apply(litSig: (String, Int)): List[FOLNode] = {
    literals.map({
      x: FOLNode => x match {
        case PositiveFOLLiteral(literal) if (litSig == (literal.top, literal.arity)) => x
        case NegativeFOLLiteral(literal) if (litSig == ("-" + literal.top, literal.arity)) => x
      }

    })

  }

  def rewrite(s : Substitution) = {
    literals.map(_.rewrite(s))
  }


  def absoluteClause: FOLClause



}

object FOLClause {

  implicit def folClause2ListFolNode(clause : FOLClause) : List[FOLNode] = clause.literals



  

  implicit def listFOLNodeToFOLALCDClause(list : List[FOLNode]) : ALCDClause = ALCDClause(list)

  //

//  implicit def listFOLNodeToStandardClause(list : List[FOLNode]) : StandardClause = StandardClause(list)


}


/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:54:46
 *
 * A standard clause == disjunction of literals == Literal OR Literal ...
 */
case class StandardClause(override val literals : List[FOLNode]) extends FOLClause {
  // all folnodes have to be literals
//  assert(literals forall ((_ match {
//    case FOLLiteral(x) => true
//    case _ => false
//  })), "FOL nodes passed into a clause can only be Literals, but literals were : %s" format (literals))

  // copy  constructor
  def this(clause : FOLClause) = this(clause.literals)


//  override def ++(that: FOLClause): FOLClause =
//    StandardClause(literals ++ that.literals)
//
//
//  override def --(that: FOLClause): FOLClause =
//    StandardClause(literals -- that.literals)
//
//
//  override def -(that: FOLNode): FOLClause =
//    StandardClause(literals - that)
//
//
//  override def +(that: FOLNode): FOLClause =
//    StandardClause(literals + that)


  override def absoluteClause = StandardClause(absoluteLiterals)


  override def toString = {
    val litStrings = literals.map({
      _ match {
//        case x if (maxLits.values.contains(x)) => x.toString + "*"
        case x => x.toString
      }
    })
    "%s" format (litStrings mkString ("[", "V", "]"))
  }


}

object StandardClause {
  def apply(params: FOLNode*): StandardClause = {
    StandardClause(List(params : _*))
  }

  def apply(clauseBuffer : ListBuffer[FOLNode]) = new StandardClause(clauseBuffer.toList)

  implicit def FOLClauseToStandardClause(x: FOLClause): StandardClause = x.asInstanceOf[StandardClause]

}



case class ALCDClause(override val literals : List[FOLNode]) extends StandardClause(literals) {
  // some more assertions
// all folnodes have to be literals
//  val isNested = literals exists ((_ match {
//    case NestedFunctionLiteral(x)  => {
//      true
//    }
//    case NestedPredicateLiteral(x)  => {
//      true
//    }
//    case _ => false
//  }))
//
//  if(isNested) {
//    error("FOL function or predicate literals passed into a ACLDFOL cannot be nested, but literals were : %s" format literals)
//  }
//
//  if(literals.size > 4) {
//    log.debug("Clause getting quite long ...")
//  }


  //require(literals.size > 0,"Literals cannot be empty , this would be the empty clause , use designated type")

 // copy  constructor
  def this(clause : FOLClause) = this(clause.literals)

}

object ALCDClause {
  def apply(params: FOLNode*): ALCDClause = {
    new ALCDClause(List(params: _*))
  }
  def apply(clause: FOLClause): ALCDClause = {
    new ALCDClause(clause)
  }
  def apply(clauseBuffer : ListBuffer[FOLNode]) = new ALCDClause(clauseBuffer.toList)
}

object SharedALCDClause {
  def apply(params: FOLNode*): ALCDClause = {
    new ALCDClause(List(params.map(_.shared) : _*))
  }
  def apply(clause: FOLClause): ALCDClause = {
    new ALCDClause(clause.literals.map(_.shared))
  }
  def apply(clauseBuffer : ListBuffer[FOLNode]) = new ALCDClause(clauseBuffer.toList.map(_.shared))
}


object EmptyClause extends FOLClause {
  override lazy val literals : List[FOLNode] = Nil

  override lazy val toString = "■"

  override lazy val isEmpty = true


  override def absoluteClause = EmptyClause

}


