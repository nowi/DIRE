package core.reduction

import containers.{ClauseStorage, CNFClauseStore}
import domain.fol.ast._
import helpers.Logging
import ordering.LiteralComparison
import org.slf4j.LoggerFactory
import rewriting.Substitution
import selection.LiteralSelection


/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 14:25:14
 */


/**
 * First-Order factoring reduces two literals to one if they are unifiable, th
 */
trait Factoring {
  def factorize(clause: FOLClause): FOLClause

  def factorize(clause: FOLClause, mgu: Option[Map[Variable, FOLNode]]): FOLClause

  def factorize(clauses: ClauseStorage): ClauseStorage

}

class OrderedFactorizer(env: {val unificator: Unify; val standardizer: Standardizing; val substitutor: Substitution; val selector: LiteralSelection; val literalComparator: LiteralComparison})
        extends Factoring
                with Logging {
  val unificator = env.unificator
  val standardizer = env.standardizer
  val substitutor = env.substitutor
  val selector = env.selector
  val literalComparator = env.literalComparator

  override def factorize(clause: FOLClause): FOLClause = {
    log.trace("Ordered Factoring by {} on clause {}", this, clause)
    // as long as there are unfiers , rewirte the clause
    // Aσ is maximal with respect to C σ ∨ Bσ
    def rule1(premise: FOLClause, a: FOLNode) = {
      // get the max

      if (premise.maxLit(literalComparator) == a) {
        log.warn("Rule 1 answering TRUE , {} is the maxLit of {} ", a, premise)
        true
      } else {
        log.warn("Rule 1 answering FALSE , {} is NOT the maxLit of {} ", a, premise)
        false

      }

    }

    //(3) nothing is selected in C σ ∨ Aσ ∨ Bσ
    def rule2(premise: FOLClause) = {
      if (selector.selectedLiterals(premise).isEmpty) {
        log.warn("Rule 2 answering TRUE , there are no selected literals in premise {} ", premise)
        true
      } else {
        log.warn("Rule 2 answering FALSE, there are selected literals in premise {} ", premise)
        false
      }
    }

    // first check literals size , has to be at least 3
    if (clause.literals.size > 2) {
      val factorizedClauses = (for (a <- clause.literals.toList;
                                    b <- clause.literals.toList;
                                    if (a != b);
                                    (aS, bS) = standardizer.standardizeApart(a, b);
                                    mgu = unificator.unify(aS, bS);
                                    if (mgu != None))

      yield {
          val clauseSubs = substitutor.substitute(mgu, clause)
          log.warn("Substitued clauses is {}", clauseSubs)
          val aSubs = substitutor.substitute(mgu, a)
          log.warn("Subs a is {}", aSubs)

          val bSubs = substitutor.substitute(mgu, b)
          log.warn("Subs B is {}", bSubs)

          if (rule1(clauseSubs, aSubs) && rule2(clauseSubs)) {
            log.warn("ORdered Factoring allowed , creating inference .. ")
            //C σ ∨ Aσ
            Some(clauseSubs - bSubs)
          } else None

        })

      log.info("Factorized clauses are {}", factorizedClauses)
      clause

    } else {
      clause
    }
  }

  def factorize(clause: FOLClause, mgu: Option[Map[Variable, FOLNode]]) = null

  def factorize(clauses: ClauseStorage): ClauseStorage = {
    assert(clauses.size == 1, "ClauseStorage has more than one clause !")
    log.trace("Ordered Factoring on clauses {} ", clauses)
    CNFClauseStore(factorize(clauses.head))
  }

}



// depends on unifier and substitutor
class StandardFactorizer(env: {val unificator: Unify; val substitutor: Substitution}) extends Factoring {
  val unificator: Unify = env.unificator
  val substitutor: Substitution = env.substitutor

  val log = LoggerFactory getLogger (this getClass)

  override def factorize(clause: FOLClause): FOLClause = {
    log.trace("Standard Factoring on clause {}", clause)
    // as long as there are unfiers , rewirte the clause
    var factorizedClause = clause
    var mgu = unificator.firstUnifier(factorizedClause)
    while (!mgu.getOrElse(Map[Variable, FOLNode]()).isEmpty) {
      factorizedClause = substitutor.substitute(mgu, factorizedClause)
      mgu = unificator.firstUnifier(factorizedClause)
    }
    factorizedClause

  }


  override def factorize(clause: FOLClause, mgu: Option[Map[Variable, FOLNode]]): FOLClause = {
    log.trace("Standard Factoring on clause {} with given mgu : {}", clause, mgu)
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
    CNFClauseStore(clauses.map(factorize(_)): _*)
  }
}



