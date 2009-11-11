package core.reduction


import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.{Variable, FOLNode, Clause}
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
  def subsumes(c1: Clause, c2: Clause): Boolean
}

class StillmannSubsumer(env: {val unificator: Unify; val substitutor: Substitution}) extends Subsumption {
  val unificator = env.unificator
  val substitutor = env.substitutor


  def subsumes(c: Clause, d: Clause): Boolean = {
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
      getMGU(a, b) match {
        case Some(t) => true
        case None => false
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
  def deleteSubsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def deleteSubsumptions(a: ClauseStorage): ClauseStorage
}

class SubsumptionDeleter(env: {val subsumptionStrategy: Subsumption}) extends SubsumptionDeletion with Logging {
  val subsumer = env.subsumptionStrategy

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


  private def subsumes(c1: Clause, c2: Clause): Boolean = {
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