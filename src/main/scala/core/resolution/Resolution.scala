package core.resolution


import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.Clause
import reduction.Factoring
import rewriting.Substitution

/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 16:15:37
 */

trait Resolution {
  def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  def resolve(a: Clause, b: Clause): Set[Clause]
}

class BinaryResolver(env: {val unificator: Unify; val factorizer: Factoring; val standardizer: Standardizing; val substitutor: Substitution}) extends Resolution {
  val unificator = env.unificator
  val factorizer = env.factorizer
  val standardizer = env.standardizer
  val substitutor = env.substitutor


  val log = net.lag.logging.Logger.get

  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.info("Resolving %s with %s by resolver : %s", a, b, this)
    // interresolve all clauses

    val resolvents1 = (for (clause1 <- a.clauses;
          clause2 <- b.clauses;
          if (clause1 != clause2);
          resolvent = resolve(clause1, clause2))
    yield resolvent)


    val resolvents2 =  resolvents1 match {
      case Nil => {
        log.info("There are no resolvents for clausestorage %s , %s", a, b)
        Set[Clause]()
      }
      case _ => {
        resolvents1.reduceLeft(_ ++ _)
      }
    }

    CNFClauseStore(resolvents2.toList)


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
    log.info("%s is resolving the Clauses %s,%s", this, a, b)

    // standardize apart the clauses
    val (aStand, bStand) = standardizer.standardizeApart(a, b)

    val aPosLits = aStand.positiveLiterals
    log.info("Positive literals of standardized Clause %s are : %s", aStand, aPosLits)
    val bNegLits = bStand.negativeLiterals
    log.info("Negative literals of standardized Clause %s are : %s", bStand, bNegLits)

    val conclusions: Set[Clause] = (for (aPos <- aPosLits;
                                         bNeg <- bNegLits;
                                         mgu = unificator.unify(aPos, bNeg))
    yield mgu match {
        case Some(x) => {
          // we have a mgu
          // apply it to the two clauses excluding the 2 focused
          log.info("MGU for Literal : %s and Literal %s is %s", aPos, bNeg, mgu)
          //            Let S_1 and S_2 be two clauses with no variables in common, let S_1 contain a positive literal L_1, S_2 contain a negative literal L_2, and let eta be the most general unifier of L_1 and L_2. Then
          //(S_1eta-L_1eta) union (S_2eta-L_2eta)  

          val S1 = substitutor.substitute(mgu, aStand) - substitutor.substitute(mgu, aPos)
          val S2 = substitutor.substitute(mgu, bStand) - substitutor.substitute(mgu, bNeg)

          val binaryResolvent = S1 ++ S2

          binaryResolvent

        }
        case None => {
          log.info("%s Could not resolve Literals %s,%s", this, aPos, bNeg)
          Clause()
        }
      }).filter(!_.literals.isEmpty) // filter out empty clauses



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
   * (i) There is a subset A′ =              { A1 , ..., Am } ⊆ A of literals all of the same sign,
   * a subset B′ =              { B1 , ..., Bn } ⊆ B of literals all of the opposite sign of the set A′ ,
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