package domain.fol.ast


import core.ordering.LiteralComparison

trait FOLClause {
  // all folnodes have to be literals
  val literals: Set[FOLNode]


  def ++(that: FOLClause): FOLClause


  def --(that: FOLClause): FOLClause


  def -(that: FOLNode): FOLClause

  def +(that: FOLNode): FOLClause

  def absoluteClause: FOLClause

  lazy val size: Int = literals.size

  // maximal literal depends on the compereator used
  protected var maxLiterals: Map[LiteralComparison, FOLNode] = Map[LiteralComparison, FOLNode]()


  def maxLit(comperator: LiteralComparison): FOLNode = {
    assert(!isEmpty, "There cannot be a max Lit in Empty Clause")
    // do lookup
    maxLiterals.get(comperator) match {
      case Some(lit) => lit
      case None => {
        // we have no max for this comparator , determine the max and cache it
        var maxLiteral = literals.toList.head

        val iter = literals.elements
        while (iter.hasNext) {
          val lit = iter.next
          comperator.compare(lit, maxLiteral) match {
            case Some(1) => maxLiteral = lit
            case Some(0) => None
            case Some(-1) => None
            case None => {
              None
            }
          }

        }
        // save this max lit
        maxLiterals += (comperator -> maxLiteral)
        maxLiteral
      }

    }

  }


  lazy val absoluteLiterals: Set[FOLNode] = {
    literals map (_ match {
      case Negation(filler) => filler
      case x: FOLNode => x

    })

  }

  lazy val positiveLiterals: Set[FOLNode] = {
    literals filter (_ match {
      case PositiveFOLLiteral(_) => true
      case _ => false

    })

  }
  lazy val negativeLiterals: Set[FOLNode] = {
    (literals filter (_ match {
      case NegativeFOLLiteral(_) => true
      case _ => false

    }))

  }


  lazy val signature = {
    literals.map({
      x: FOLNode => x match {
        case PositiveFOLLiteral(literal) => (literal.symbolicName, literal.arity)
        case NegativeFOLLiteral(literal) => ("-" + literal.symbolicName, literal.arity)
      }

    })

  }


  /**Retrieve the literals which is associated with the given literal signature.
   * @param litSig the signature of the literal (symblicname,arity)
   * @return the literals
   */
  def apply(litSig: (String, Int)): Set[FOLNode] = {
    literals.map({
      x: FOLNode => x match {
        case PositiveFOLLiteral(literal) if (litSig == (literal.symbolicName, literal.arity)) => x
        case NegativeFOLLiteral(literal) if (litSig == ("-" + literal.symbolicName, literal.arity)) => x
      }

    })

  }


  lazy val isEmpty = literals.isEmpty

  lazy val isUnit = literals.size == 1

  // A Horn clause is a disjunction of literals of which at most one is
  // positive.
  lazy val isHorn = !isEmpty && positiveLiterals.size <= 1

  lazy val isDefinitive = !isEmpty && positiveLiterals.size == 1

}

/**
 * User: nowi
 * Date: 07.10.2009
 * Time: 15:54:46
 *
 * A standard clause == disjunction of literals == Literal OR Literal ...
 */
case class StandardClause(literals: Set[FOLNode]) extends FOLClause {
  // all folnodes have to be literals
  assert(literals forall ((_ match {
    case FOLLiteral(x) => true
    case _ => false
  })), "FOL nodes passed into a clause can only be Literals, but literals were : %s" format (literals))






  override def ++(that: FOLClause): FOLClause =
    StandardClause(literals ++ that.literals)


  override def --(that: FOLClause): FOLClause =
    StandardClause(literals -- that.literals)


  override def -(that: FOLNode): FOLClause =
    StandardClause(literals - that)


  override def +(that: FOLNode): FOLClause =
    StandardClause(literals + that)


  override def absoluteClause = StandardClause(absoluteLiterals)


  override def toString = {
    val litStrings = literals.map({
      _ match {
        case x if (maxLiterals.values.contains(x)) => x.toString + "+"
        case x => x.toString
      }
    })
    "%s" format (litStrings mkString ("[", "∨", "]"))
  }


}

case class EmptyClause extends FOLClause {
  override val literals = Set[FOLNode]()

  override def toString = "■"

  override lazy val isEmpty = true


  override def absoluteClause = EmptyClause()

  override def +(that: FOLNode) = EmptyClause()

  override def -(that: FOLNode) = EmptyClause()

  override def ++(that: FOLClause) = EmptyClause()

  override def --(that: FOLClause) = EmptyClause()
}

object StandardClause {
  def apply(params: FOLNode*): StandardClause = {
    StandardClause(Set(params: _*))
  }

  implicit def FOLClauseToStandardClause(x: FOLClause): StandardClause = x.asInstanceOf[StandardClause]

}


