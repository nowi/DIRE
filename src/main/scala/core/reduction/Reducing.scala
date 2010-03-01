package core.reduction


import collection.mutable.ListBuffer
import containers.{NonEmptyClauseStore, CNFClauseStore, ClauseStorage}
import domain.fol.ast._
import helpers.Logging
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
    require(c.literals.size > 0 && d.literals.size > 0,"Argument clauses to Stillman algorithm cannot be empty")
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


  def subsumptions(inClauses: ClauseStorage, fromClauses: ClauseStorage): ClauseStorage


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
    (a, b) match {
      case (NonEmptyClauseStore(inClauses), NonEmptyClauseStore(fromClauses)) => {
        log.trace("Delete Subsumptions in Clauses %s that are subsumed by %s ...  for now we check if a is contained in b", inClauses, fromClauses)
        val subsumed = new ListBuffer[FOLClause]

        for (c1 <- fromClauses;
             c2 <- inClauses;
             if (c1 != c2);
             if (!c1.isEmpty && !c2.isEmpty);
             if (subsumer.subsumes(c1, c2))) {
          subsumed.append(c2)
        }
        log.trace("2. Subsumption deletion identified subsumed clauses : %s ", subsumed)

        // return clauses that where not subsumed

        inClauses.filterClauses {!subsumed.contains(_)}
      }
      case _ => {
        log.debug("Passed in empty clauseStorage into subsumtpion deletion method")
        a
      }
    }


  }

  override def subsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    (a, b) match {
      case (NonEmptyClauseStore(inClauses), NonEmptyClauseStore(fromClauses)) => {
        log.trace("Delete Subsumptions in Clauses %s that are subsumed by %s ...  for now we check if a is contained in b", inClauses, fromClauses)
        val subsumed = (for (c1 <- fromClauses; c2 <- inClauses; if (c1 != c2); if (!c1.isEmpty && !c2.isEmpty); if (subsumer.subsumes(c1, c2))) yield c2)
        log.trace("2. Subsumption deletion identified subsumed clauses : %s ", subsumed)
        CNFClauseStore(subsumed: _*)
      }
      case _ => {
        log.debug("Passed in empty clauseStorage into subsumtpion deletion method")
        a
      }
    }


  }

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  override def deleteSubsumptions(c: ClauseStorage): ClauseStorage = {
    log.trace("Subsumption deletion to all of %s", c)
    c match {
      case NonEmptyClauseStore(clauses: ClauseStorage) => {
        // interreduce all claueses
        val buffer = new ListBuffer[FOLClause]
        for (c1 <- clauses;
             c2 <- clauses;
             if (c1 != c2);
             if (subsumer.subsumes(c1, c2))) {
          buffer.append(c2)
        }
        val subsumed = buffer.toList
        log.debug("1. Subsumption deletion identified subsumed clauses : %s ", buffer)


        // return clauses that where not subsumed

        c.filterClauses {!subsumed.contains(_)}


      }


      case _ => c
    }


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
          log.debug("not a subset regarding symbolicnames and arity")
          false
        }

      } else {
        // is not smaller , in groundcase this means cannot subsume
        log.debug("is not smaller , in groundcase this means cannot subsume")
        false

      }
    } else {
      log.debug("they are equal")
      false
    }
  }

}


trait TautologyDeletion extends Reducing {
  def deleteTautologies(clauses: ClauseStorage): ClauseStorage


}

class TautologyDeleter extends TautologyDeletion with Logging {
  override def deleteTautologies(clauses: ClauseStorage): ClauseStorage = {
    clauses match {
      case NonEmptyClauseStore(clauseStore) => {
        log.trace("Tautology elemination on %s by %s", clauseStore, this)
        clauseStore.filterClauses {!isTautology(_)}

      }

      case _ => {
        log.debug("1. Tautlogy eleimation skipped on clause store : %s", clauses)
        clauses
      }
    }

  }


  private def isTautology(clause: FOLClause): Boolean = {

    val negLits = clause.negativeLiterals.asInstanceOf[Set[Negation]].map({x: Negation => x.filler})
    val posLits = clause.positiveLiterals

    val tautLits: Set[Boolean] = for (nLit <- negLits; pLit <- posLits)
    yield (pLit == nLit)
    (Set(false) ++ tautLits).reduceLeft(_ || _)
  }


  override def reduce(a: ClauseStorage) = deleteTautologies(a)
}