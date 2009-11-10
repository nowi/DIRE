package core.reduction


import containers.{CNFClauseStore, ClauseStorage}
import org.slf4j.LoggerFactory

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 15:19:43
 */

trait Reducing {
  def reduce(a: ClauseStorage): ClauseStorage
}


trait SubsumptionDeletion extends Reducing {
  def deleteSubsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def deleteSubsumptions(a: ClauseStorage): ClauseStorage
}

class SubsumptionDeleter extends SubsumptionDeletion {
  private val log = LoggerFactory getLogger (this getClass)

  override def reduce(a: ClauseStorage) = deleteSubsumptions(a)

  override def deleteSubsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Delete Subsumptions {} with {} ...  for now we check if a is contained in b", a, b)
    // remove the intersection
    a

  }

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  override def deleteSubsumptions(a: ClauseStorage): ClauseStorage = {
    log.trace("Subsumption deletion to all of {}", a)
    a
  }
}


trait TautologyDeletion extends Reducing {
  def deleteTautologies(clauses: ClauseStorage): ClauseStorage


}

class TautologyDeleter extends TautologyDeletion {
  private lazy val log = LoggerFactory getLogger (this getClass)

  override def deleteTautologies(clauses: ClauseStorage): ClauseStorage = {
    log.trace("Tautology elemination on {} by {}", clauses, this)
    clauses
  }


  override def reduce(a: ClauseStorage) = deleteTautologies(a)
}