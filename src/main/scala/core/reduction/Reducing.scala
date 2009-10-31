package core.reduction


import containers.{CNFClauseStore, ClauseStorage}
import net.lag.logging.Logger

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 15:19:43
 */

trait Reducing {
}


trait SubsumptionDeletion extends Reducing {
  private lazy val log = Logger.get

  def deleteSubsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.info("Subsume %s with %s", a, b)
    CNFClauseStore()

  }

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def deleteSubsumptions(a: ClauseStorage): ClauseStorage = {
    log.info("Subsumption deletion to all of %s", a)
    CNFClauseStore()
  }
}

trait TautologyDeletion extends Reducing {
  private lazy val log = Logger.get

  def deleteTautologies(clauses: ClauseStorage): ClauseStorage = {
    log.info("Tautology elemination on %s by %s", clauses, this)
    CNFClauseStore()
  }
}