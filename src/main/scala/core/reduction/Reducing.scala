package core.reduction


import containers.{CNFClauseStore, ClauseStorage}
import net.lag.logging.Logger

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
  private val log = Logger.get

  override def reduce(a: ClauseStorage) = deleteSubsumptions(a)

  override def deleteSubsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.info("Subsume %s with %s", a, b)
    CNFClauseStore()

  }

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  override def deleteSubsumptions(a: ClauseStorage): ClauseStorage = {
    log.info("Subsumption deletion to all of %s", a)
    CNFClauseStore()
  }
}


trait TautologyDeletion extends Reducing {
  def deleteTautologies(clauses: ClauseStorage): ClauseStorage


}

class TautologyDeleter extends TautologyDeletion {
  private lazy val log = Logger.get

  override def deleteTautologies(clauses: ClauseStorage): ClauseStorage = {
    log.info("Tautology elemination on %s by %s", clauses, this)
    CNFClauseStore()
  }


  override def reduce(a: ClauseStorage) = deleteTautologies(a)
}