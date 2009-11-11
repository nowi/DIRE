package core.reduction


import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast._
import helpers.Logging
import org.slf4j.LoggerFactory
import rewriting.Substitution

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 15:19:43
 */

trait Reducing {
  def reduce(a: ClauseStorage): ClauseStorage
}

trait Subsumption {
  def subsumes(c1: FOLClause, c2: FOLClause): Boolean
}

class StillmannSubsumer(env: {val unificator: Unify; val substitutor: Substitution}) extends Subsumption {
  val unificator = env.unificator
  val substitutor = env.substitutor


  def subsumes(c: FOLClause, d: FOLClause): Boolean = {
    //    Let C = (L,, . . . , L,) and D = (Ki, . . . , Km).
    // init the map
    val emptyTheta = Map[Variable, FOLNode]()
    // fix the clause literals as lists
    val cLits = c.literals.toList
    val dLits = d.literals.toList

    def st(i: Int, j: Int, theta: Map[Variable, FOLNode]): Boolean = {
      if (j < d.size) {
        var a = j
        val thetaLi = subs(theta, cLits(i))
        while (a < d.size && !doesUnify(thetaLi, dLits(a))) {
          a = a + 1
        }
        if (a < d.size) {
          val mgu = getMGU(thetaLi, dLits(a))
          if (i + 1 == c.size || st(i + 1, 1, theta ++ mgu.getOrElse(Map[Variable, FOLNode]()))) {
            true
          } else {
            st(i, a + 1, theta)
          }
        } else {
          false
        }

      } else {
        false
      }
    }

    def doesUnify(a: FOLNode, b: FOLNode): Boolean = {
      def checkSubstitution(a: FOLNode, b: FOLNode): Boolean = {
        getMGU(a, b) match {
        // nonempty
          case Some(t: Map[Variable, FOLNode]) if (!t.isEmpty) => { // if there was a substitution
            // there is a unifier
            // now check if this unfier did apply to the node if there was a unifier
            if (!a.flatArgs.intersect(t.keys.toList).isEmpty) {
              true
            } else {
              false
            }
          }

          case Some(t: Map[Variable, FOLNode]) if (t.isEmpty) => { // empty theta == no substitution
            true
          }
          case None => false
        }

      }
      // take the order of the arugments into consideration

      // we can never unify a positive with a negative literal
      (a, b) match {
        case (PositiveFOLLiteral(x), PositiveFOLLiteral(y)) => checkSubstitution(a, b)
        case (NegativeFOLLiteral(x), NegativeFOLLiteral(y)) => checkSubstitution(a, b)
        case _ => false
      }


    }

    def getMGU(a: FOLNode, b: FOLNode) = {
      unificator.unify(a, b)
    }

    def subs(theta: Map[Variable, FOLNode], b: FOLNode): FOLNode = substitutor.substitute(Some(theta), b)

    st(0, 0, emptyTheta)

  }
}


trait SubsumptionDeletion extends Reducing {
  /**
   * Apply subsumption deletion to all clauses from the inClauses that are subsumed by clauses
   * from the fromClauses store
   * @param inClauses : The clauses that should have subsumed clauses removed
   * @param fromClauses : The clauses that should be subsumed agains
   * @return the resulting clausestore
   */
  def deleteSubsumptions(inClauses: ClauseStorage, fromClauses: ClauseStorage): ClauseStorage

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def deleteSubsumptions(a: ClauseStorage): ClauseStorage
}

class SubsumptionDeleter(env: {val subsumptionStrategy: Subsumption}) extends SubsumptionDeletion with Logging {
  val subsumer = env.subsumptionStrategy

  override def reduce(a: ClauseStorage) = deleteSubsumptions(a)

  override def deleteSubsumptions(inClauses: ClauseStorage, fromClauses: ClauseStorage): ClauseStorage = {
    log.trace("Delete Subsumptions in Clauses {} that are subsumed by {} ...  for now we check if a is contained in b", inClauses, fromClauses)
    val subsumed =
    for (c1 <- fromClauses.clauses; c2 <- inClauses.clauses; if (c1 != c2); if (subsumer.subsumes(c1, c2))) yield c2
    log.trace("Subsumption deletion identified subsumed clauses : {} ", subsumed)
    inClauses -- CNFClauseStore(subsumed)

  }

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  override def deleteSubsumptions(a: ClauseStorage): ClauseStorage = {
    log.trace("Subsumption deletion to all of {}", a)
    // interreduce all claueses
    val subsumed =
    for (c1 <- a.clauses; c2 <- a.clauses; if (c1 != c2); if (subsumer.subsumes(c1, c2))) yield c2
    log.trace("Subsumption deletion identified subsumed clauses : {} ", subsumed)
    a -- CNFClauseStore(subsumed)

  }


  private def subsumes(c1: FOLClause, c2: FOLClause): Boolean = {
    if (c1 != c2) {
      // Filter 1
      // Ensure c1 has less literals total and that
      // it is a subset of the c2 clauses -- positive and negative counts
      if (c1.literals.size <= c2.literals.size
              && c1.positiveLiterals.size <= c2.positiveLiterals.size
              && c1.negativeLiterals.size <= c2.negativeLiterals.size) {


        // FILTER 2
        // Check if c1  symbolname/arity are a subset of the
        // c2 clause literal/arity names
        if (c1.signature subsetOf c2.signature) {
          // check with the plugged in subsumption checking strategy
          subsumer.subsumes(c1, c2)

        } else {
          // not a subset regarding symbolicnames and arity
          log.info("not a subset regarding symbolicnames and arity")
          false
        }

      } else {
        // is not smaller , in groundcase this means cannot subsume
        log.info("is not smaller , in groundcase this means cannot subsume")
        false

      }
    } else {
      log.info("they are equal")
      false
    }
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