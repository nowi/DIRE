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

  def factorize(clause: Clause, mgu: Option[MGU]): Clause

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


  def factorize(clause: Clause, mgu: Option[MGU]) = null

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

    // permutate all literals in the clause  with each other
    // collect the substitutions
    // we need a global theta for this
    var substitutions: Map[Variable, FOLNode] = Map[Variable, FOLNode]()
    var rewrittenClause = clause

    // unify all literals of these clauess
    val thetas = for (l1 <- clause.literals;
                      l2 <- clause.literals;
                      if (l1 != l2)
    ) {
        unificator.unify(l1, l2, Some(substitutions)) match {
          case Some(x) => {
            // there was a unification apply the rewrite
            log.info("Success unifying literals : %s with %s ...", l1, l2)
            log.info("New global theta is : %s ", x)
            substitutions = x
          }
          case None => {
            // ignore
            log.info("Could not unify literal : %s with %s ... ignoring", l1, l2)
          }

        }
      }


    log.info("Final global theta : %s", substitutions);

    log.info("COllected Substitutions during factoring : %s", substitutions)

    // apply the substitutions to the clause if there are any
    if (!substitutions.isEmpty) {
      log.info("Found unifiers .. applying substitutions to clause : %s ", clause)
      substitutor.substitute(Some(substitutions), clause)
    } else {
      log.info("Not found any substitutions for clause : %s ", clause)
      clause
    }

  }


  override def factorize(clause: Clause, mgu: Option[MGU]): Clause = {
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



