package core.resolution


import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.Clause
import reduction.Factoring

/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 16:15:37
 */

trait Resolution {
  def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  def resolve(a: Clause, b: Clause): Set[Clause]
}

class BinaryResolver(env: {val unificator: Unify; val factorizer: Factoring}) extends Resolution {
  val unificator = env.unificator
  val factorizer = env.factorizer

  val log = net.lag.logging.Logger.get

  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.info("Resolving %s with %s by resolver : %s", a, b, this)
    a
  }


  /**
   * Binary resolution always focuses on two clauses and one literal in each.
   * To admit a conclusion, the literals must be opposite in sign and alike in predicate,
   * and there must exist a unifier (substitution of terms for variables) to otherwise make them
   * identical. If a conclusion results, it is obtained by applying the unifier to the two
   * clauses excluding the two literals in focus, and taking the union of the transformed literals.
   */
  override def resolve(a: Clause, b: Clause): Set[Clause] = {
    // Apos , Bneg
    log.info("%s is resolving Clauses %s,%s", this, a, b)
    val aPosLits = a.positiveLiterals
    val bNegLits = b.negativeLiterals

    val conclusions: Set[Clause] = for (aPos <- aPosLits;
                                        bNeg <- bNegLits;
                                        mgu = unificator.unify(aPos, bNeg).asInstanceOf[Option[MGU]])
    yield mgu match {
        case Some(x) => {
          // we have a mgu
          // apply it to the two clauses excluding the 2 focused
          log.info("MGU is %s", mgu)
          //            val c1 = factorizer.factorize(Clause(aPosLits - aPos), mgu)
          val c1 = factorizer.factorize(Clause(aPosLits - aPos))
          //            val c2 = factorizer.factorize(Clause(bNegLits - bNeg), mgu)
          val c2 = factorizer.factorize(Clause(bNegLits - bNeg))

          val union = c1 ++ c2
          log.info("Resolved for Literals %s,%s  --> %s %s , by factoring with mgu : %s", this, aPos, bNeg, c1, c2, mgu)
          union
        }
        case None => {
          log.info("%s Could not resolve Literals %s,%s", this, aPos, bNeg)
          Clause()
        }
      }



    log.info("Resolver %s resolved from clauses %s and %s --> %s", this, a, b, conclusions)
    conclusions

  }


}

class GeneralResolution(env: {val unificator: Unify}) extends Resolution {
  val unificator = env.unificator

  val log = net.lag.logging.Logger.get

  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.info("Resolving %s with %s by resolver : %s", a, b, this)
    a
  }


  /**
   * Deﬁnition 8.5.2 Given two clauses A and B, a clause C is a resolvent of
   * A and B iﬀ the following holds:
   *
   * (i) There is a subset A′ =       { A1 , ..., Am } ⊆ A of literals all of the same sign,
   * a subset B′ =       { B1 , ..., Bn } ⊆ B of literals all of the opposite sign of the set A′ ,
   * and a separating pair of substitutions (ρ, ρ′ ) such that the set |ρ(A′ ) ∪ ρ′ (B′ )|
   * is uniﬁable;
   *
   * (ii) For some most general uniﬁer σ of the set |ρ(A′ ) ∪ ρ′ (B′ )|, we have
   * C = σ(ρ(A − A′ ) ∪ ρ′ (B − B′ )).
   */
  override def resolve(a: Clause, b: Clause): Set[Clause] = {
    Set(a)
  }


}