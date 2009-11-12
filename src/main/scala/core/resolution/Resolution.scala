package core.resolution


import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.{FOLNode, FOLClause, EmptyClause, Clause}
import org.slf4j.LoggerFactory
import reduction.Factoring
import rewriting.Substitution

/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 16:15:37
 */

trait Resolution {
  def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  def resolve(a: FOLClause, b: FOLClause): Set[FOLClause]
}

class BinaryResolver(env: {val unificator: Unify; val factorizer: Factoring; val standardizer: Standardizing; val substitutor: Substitution}) extends Resolution {
  val unificator = env.unificator
  val factorizer = env.factorizer
  val standardizer = env.standardizer
  val substitutor = env.substitutor


  val log = LoggerFactory getLogger (this getClass)

  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Resolving {} with {}", a, b)
    // resolve

    val resolvents1 = (for (clause1 <- a.clauses;
                            clause2 <- b.clauses;
                            if (clause1 != clause2);
                            resolvent = resolve(clause1, clause2))
    yield resolvent)


    val resolvents2 = resolvents1 match {
      case x if (x.isEmpty) => {
        log.info("There are no resolvents for clausestorage {} , {}", a, b)
        Set[FOLClause]()
      }
      case _ => {
        resolvents1.reduceLeft(_ ++ _)
      }
    }

    CNFClauseStore(resolvents2)


  }


  /**
   * Binary resolution always focuses on two clauses and one literal in each.
   * To admit a conclusion, the literals must be opposite in sign and alike in predicate,
   * and there must exist a unifier (substitution of terms for variables) to otherwise make them
   * identical. If a conclusion results, it is obtained by applying the unifier to the two
   * clauses excluding the two literals in focus, and taking the union of the transformed literals.
   */
  override def resolve(a: FOLClause, b: FOLClause): Set[FOLClause] = {
    log.trace("Resolving the Clauses {},{}", a, b)
    // standardize apart the clauses
    val (aStand, bStand) = standardizer.standardizeApart(a, b)

    // Apos , Bneg

    val conclusions = doResolve(aStand, bStand) ++
            doResolve(bStand, aStand)

    if (!conclusions.isEmpty) {
      log.trace("Resolved from clauses {} and {} --> {}", (a, b, conclusions))
    } else {
      log.trace("RESOLVED NOTHING from clauses {} and {} !", a, b)
    }
    conclusions

  }


  private def doResolve(aStand: FOLClause, bStand: FOLClause): Set[FOLClause] = {

    val aLits = aStand.positiveLiterals
    //    log.trace("Positive literals of standardized Clause {} are : {}", aStand, aLits)
    val bLits = bStand.negativeLiterals
    //    log.trace("Negative literals of standardized Clause {} are : {}", bStand, bLits)

    val conclusions: Set[FOLClause] = (for (aPos <- aLits;
                                            bNeg <- bLits;
                                            mgu = unificator.unify(aPos, bNeg);
                                            if (mgu != None))
    yield mgu match {
        case Some(x) => {
          // we have a mgu
          // apply it to the two clauses excluding the 2 focused
          log.trace("MGU for Literal : {} and Literal {} is {}", Array(aPos, bNeg, mgu))
          //            Let S_1 and S_2 be two clauses with no variables in common, let S_1 contain a positive literal L_1, S_2 contain a negative literal L_2, and let eta be the most general unifier of L_1 and L_2. Then
          //(S_1eta-L_1eta) union (S_2eta-L_2eta)

          val S1 = substitutor.substitute(mgu, aStand)
          val aPosS = substitutor.substitute(mgu, aPos)
          val S2 = substitutor.substitute(mgu, bStand)
          val bNegS = substitutor.substitute(mgu, bNeg)


          val binaryResolvent = (S1 - aPosS) ++ (S2 - bNegS)

          if (binaryResolvent.isEmpty) {
            log.info("EMPTY CLAUSE RESOLVED FROM {} and {}", aStand, bStand)
            EmptyClause()
          } else {
            binaryResolvent
          }

        }
        case None => {
          log.trace("Could not resolve Literals {},{}", aPos, bNeg)
          Clause()
        }
      })

    conclusions

  }


}

class GeneralResolution(env: {val unificator: Unify}) extends Resolution {
  val unificator = env.unificator

  val log = LoggerFactory getLogger (this getClass)

  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Resolving {} with {}", a, b)
    a
  }


  /**
   * Deﬁnition 8.5.2 Given two clauses A and B, a clause C is a resolvent of
   * A and B iﬀ the following holds:
   *
   * (i) There is a subset A′ =                       { A1 , ..., Am } ⊆ A of literals all of the same sign,
   * a subset B′ =                       { B1 , ..., Bn } ⊆ B of literals all of the opposite sign of the set A′ ,
   * and a separating pair of substitutions (ρ, ρ′ ) such that the set |ρ(A′ ) ∪ ρ′ (B′ )|
   * is uniﬁable;
   *
   * (ii) For some most general uniﬁer σ of the set |ρ(A′ ) ∪ ρ′ (B′ )|, we have
   * C = σ(ρ(A − A′ ) ∪ ρ′ (B − B′ )).
   */
  override def resolve(a: FOLClause, b: FOLClause): Set[FOLClause] = {
    Set(a)
  }


}