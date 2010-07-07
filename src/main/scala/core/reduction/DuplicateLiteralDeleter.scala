package de.unima.dire.core.reduction


import collection.mutable.ListBuffer
import de.unima.dire.domain.fol.ast.{FOLNode}
import de.unima.dire.helpers.Logging
import de.unima.dire.core.containers.{FOLClause,ALCDClause}

/**
 * User: nowi
 * Date: 25.04.2010
 * Time: 11:29:56
 */
trait DuplicateLiteralDeletion {
  def apply(clauseBuffer: Set[FOLNode]): Set[FOLNode]

}


class DuplicateLiteralDeleter extends DuplicateLiteralDeletion with Logging {
  override def apply(clauseBuffer: Set[FOLNode]): Set[FOLNode] = {
    log.warning("Duplicate literal deletion is redundant")
    // make literals unique
//    val condensed = clauseBuffer.distinct
//
//    if (clauseBuffer.size - condensed.size > 0) {
//      log.debug("%s condensed clause %s --> %s", this, clauseBuffer, condensed)
//    }
//    condensed

    clauseBuffer

  }
}