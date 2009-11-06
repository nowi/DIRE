package core.reduction

import containers.{ClauseStorage, CNFClauseStore}
import domain.fol.ast._
import net.lag.logging.Logger
import rewriting.Substitution


/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 14:25:14
 */


/**
 * First-Order factoring reduces two literals to one if they are unifiable, th
 */
trait Factoring {
  def factorize(clause: Clause): Clause

  def factorize(clause: Clause, mgu: Option[Map[Variable, FOLNode]]): Clause

  def factorize(clauses: ClauseStorage): ClauseStorage

}

class OrderedFactorizer(env: {val unificator: Unify})
        extends Factoring {
  // depends on unifier

  val log = Logger.get

  def factorize(clause: Clause): Clause = {
    log.info("Ordered Factoring on clause %s ", clause)
    clause

  }


  def factorize(clause: Clause, mgu: Option[Map[Variable, FOLNode]]) = null

  def factorize(clauses: ClauseStorage): ClauseStorage = {
    log.info("Ordered Factoring on clauses %s ", clauses)
    clauses
  }

}



// depends on unifier and substitutor
class StandardFactorizer(env: {val unificator: Unify; val substitutor: Substitution}) extends Factoring {
  val unificator: Unify = env.unificator
  val substitutor: Substitution = env.substitutor

  val log = Logger.get

  override def factorize(clause: Clause): Clause = {
    log.info("Standard Factoring on clause %s", clause)
    // as long as there are unfiers , rewirte the clause
    var factorizedClause = clause
    var mgu = unificator.firstUnifier(factorizedClause)
    while (!mgu.getOrElse(Map[Variable, FOLNode]()).isEmpty) {
      factorizedClause = substitutor.substitute(mgu, factorizedClause)
      mgu = unificator.firstUnifier(factorizedClause)
    }
    factorizedClause

  }


  override def factorize(clause: Clause, mgu: Option[Map[Variable, FOLNode]]): Clause = {
    log.info("Standard Factoring on clause %s with given mgu : %s", clause, mgu)
    mgu match {
      case Some(x) => {
        // apply the factorization on this clause with the given mgu
        substitutor.substitute(mgu, clause)
      }
      case None => {
        throw new IllegalArgumentException("No mgu specified !")
        clause
      }
    }
  }

  override def factorize(clauses: ClauseStorage): ClauseStorage = {
    CNFClauseStore(clauses.clauses.map(factorize(_)))
  }
}



