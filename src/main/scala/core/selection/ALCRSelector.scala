package core.selection


import domain.fol.ast._


/**
 * User: nowi
 * Date: 14.12.2009
 * Time: 18:26:17
 */

class ALCRSelector extends LiteralSelection {
  def isSelected(atom: FOLNode, clause: StandardClause) = {
    selectedLiterals(clause).contains(atom)
  }

  def selectedLiterals(clause: StandardClause) = {
    // select negative binary predicates only
    clause.literals.toList.filter {
      x: FOLNode => x match {
        case Negation(filler: Predicate) if (filler.arity > 1) => {
          true
        }
        case Negation(filler: Function) if (filler.arity > 1) => {
          true
        }
        case _ => false

      }
    }
  }
}