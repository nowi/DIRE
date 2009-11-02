package core.reduction

import containers.{ClauseStorage, CNFClauseStore}
import domain.fol.ast._
import net.lag.logging.Logger

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

  def factorize(clauses: ClauseStorage): ClauseStorage
}

trait OrderedFactoring extends Factoring with Unify {
  override val log = Logger.get

  def factorize(clause: Clause): Clause = {
    log.info("Ordered Factoring on clause %s ", clause)
    clause

  }

  def factorize(clauses: ClauseStorage): ClauseStorage = {
    log.info("Ordered Factoring on clauses %s ", clauses)
    clauses
  }

}



trait StandardFactoring extends Factoring with Unify {
  override val log = Logger.get

  override def factorize(clause: Clause): Clause = {
    log.info("Standard Factoring on clause %s", clause)

    // permutate all literals in the clause  with each other
    // collect the substitutions
    val substitutions = for (
      c1 <- clause.literals;
      c2 <- clause.literals;
      unifier = unify(c1, c2);
      if (c1 != c2)

    )
    yield unifier match {
        case Some(x) => {
          log.info("Literal : %s unifies with %s ... mgu : %s", c1, c2, x)
          x
        }
        case _ => Map[Variable, FOLNode]()

      }

    log.info("COllected Substitutions during factoring : %s", substitutions)

    // apply the substitutions to the clause if there are any
    if (!substitutions.isEmpty) {
      log.info("Found unifiers .. applying substitutions to clause : %s ", clause)
      substitute(Some(substitutions.reduceLeft(_ ++ _)), clause)
    } else {
      log.info("Not found any substitutions for clause : %s ", clause)
      clause
    }

  }

  override def factorize(clauses: ClauseStorage): ClauseStorage = {
    log.info("Factoring on clauses %s ", clauses)
    clauses
  }
}

class StandardFactorizer extends StandardFactoring {
}


