package core.selection


import domain.fol.ast._


/**
 * User: nowi
 * Date: 14.12.2009
 * Time: 18:26:17
 */

class DALCRSelector extends LiteralSelection {
  def isSelected(atom: FOLNode, clause: StandardClause) = {
    selectedLiterals(clause).contains(atom)
  }

  def selectedLiterals(clause: StandardClause) = {
    // select negative binary predicates only
    val selectedLits = clause.literals.toList.filter {
      x: FOLNode => x match {
        case Negation(filler: Predicate) if (filler.arity == 2 ) => {
          true
        }
        case Negation(filler: Function) if (filler.arity == 2) => {
          true
        }
        case _ => false

      }
    }

     /*
    Corollary 1.
    Saturating an ALC knowledge base by RDL ,
    1) the selection function never selects more than one literal and
    2) the order is total on the literals of a clause that are not selected.
     */
    if((selectedLits.size > 1)){
      throw new IllegalStateException("Clause cannot have more then 1 selected literals")

    }

    selectedLits



  }
}