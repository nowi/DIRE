package core.reduction


import collection.mutable.ListBuffer
import domain.fol.ast.{FOLNode, ALCDClause, FOLClause}
import helpers.Logging

/**
 * User: nowi
 * Date: 25.04.2010
 * Time: 11:29:56
 */
trait DuplicateLiteralDeletion {
  def apply(clauseBuffer: Set[FOLNode]): Set[FOLNode]

}


object DuplicateLiteralDeleter extends DuplicateLiteralDeletion with Logging {
  override def apply(clauseBuffer: Set[FOLNode]): Set[FOLNode] = {
    log.warning("Duplicate literal deletion is redundant")
    // make literals unique
//    val condensed = clauseBuffer.removeDuplicates
//
//    if (clauseBuffer.size - condensed.size > 0) {
//      log.debug("%s condensed clause %s --> %s", this, clauseBuffer, condensed)
//    }
//    condensed

    clauseBuffer

  }
}