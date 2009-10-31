package core.inference

import containers.{ClauseStorage, CNFClauseStore}
import domain.fol.ast._
import net.lag.logging.Logger

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 14:25:14
 */

trait Inferencing {
  // TODO Implement this
  def infer(clause: Clause): Clause
}

class Resolution extends Inferencing {
  // TODO Implement this

  def infer(clause: Clause) = null
}


trait Factoring {
  def factorize(clause: Clause): Clause

  def factorize(clauses: ClauseStorage): ClauseStorage
}

trait OrderedFactoring extends Factoring {
  private lazy val log = Logger.get

  def factorize(clause: Clause): Clause = {
    log.info("Ordered Factoring on clause %s ", clause)
    clause

  }

  def factorize(clauses: ClauseStorage): ClauseStorage = {
    log.info("Ordered Factoring on clauses %s ", clauses)
    clauses
  }

}



trait StandardFactoring extends Factoring {
  private lazy val log = Logger.get

  override def factorize(clause: Clause): Clause = {
    log.info("Factoring on clause %s ", clause)
    clause

  }

  override def factorize(clauses: ClauseStorage): ClauseStorage = {
    log.info("Factoring on clauses %s ", clauses)
    clauses
  }
}


