package core.selection


import domain.fol.ast.{StandardClause, FOLNode}

/**
 * User: nowi
 * Date: 24.11.2009
 * Time: 17:15:01
 */

trait LiteralSelection {
  def selectedLiterals(clause: StandardClause): List[FOLNode]

  def isSelected(atom: FOLNode, clause: StandardClause): Boolean
}

/**
 * A standard clause selection function 
 */
class NegativeLiteralsSelection extends LiteralSelection {
  def selectedLiterals(clause: StandardClause) = {
    // return the set of nonempty sequences of negative atoms of the standard clause
    clause.negativeLiterals.toList
  }

  def isSelected(atom: FOLNode, clause: StandardClause) = {
    //    clause.negativeLiterals.toList contains atom
    clause.negativeLiterals.toList.contains(atom)

  }


  //  def selectedLiterals(clause: StandardClause) = {
  //    // return the set of nonempty sequences of negative atoms of the standard clause
  //    Set[List[FOLNode]]() ++
  //            comb(clause.literals.size,clause.literals.toList)
  //
  //
  //  }
  //
  //  def mycomb[T](n: Int, l: List[T]): List[List[T]] =
  //    n match {
  //      case 0 => List(List())
  //      case _ => for (el <- l;
  //                     sl <- mycomb(n - 1, l dropWhile {_ != el}))
  //      yield (el :: sl).removeDuplicates
  //    }
  //
  //  def comb[T](n: Int, l: List[T]): List[List[T]] = mycomb(n, l.removeDuplicates)

}