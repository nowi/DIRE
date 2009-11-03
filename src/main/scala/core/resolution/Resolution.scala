package core.resolution


import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.Clause

/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 16:15:37
 */

trait Resolution {
  def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  def resolve(a: Clause, b: Clause): Set[Clause]
}

class BinaryResolver(env: {val unificator: Unify}) extends Resolution {
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
   * (i) There is a subset A′ =  { A1 , ..., Am } ⊆ A of literals all of the same sign,
   * a subset B′ =  { B1 , ..., Bn } ⊆ B of literals all of the opposite sign of the set A′ ,
   * and a separating pair of substitutions (ρ, ρ′ ) such that the set |ρ(A′ ) ∪ ρ′ (B′ )|
   * is uniﬁable;
   *
   * (ii) For some most general uniﬁer σ of the set |ρ(A′ ) ∪ ρ′ (B′ )|, we have
   * C = σ(ρ(A − A′ ) ∪ ρ′ (B − B′ )).
   */
  override def resolve(a: Clause, b: Clause): Set[Clause] = {
    //    log.info("Resolving %s with %s by resolver : %s", a, b, this)
    //
    //
    //    // first case
    //    val mgu = unificator.unify() .positiveLiterals ++ b.negativeLiterals
    //
    //
    //
    //
    //
    //    val aneg = a.negativeLiterals
    //    val bpos = b.positiveLiterals
    //
    //
    //
    //
    //
    //
    //    binaryResolvents(a)
    Set(a)
  }
  //
  //  def binaryResolvents(a : Clause ,b : Clause) : Option[Set[Clause][] = {
  //    Set<Clause> resolvents = new LinkedHashSet<Clause>();
  //    if (a.isEmpty && b.isEmpty) {
  //      None
  //    } else {
  //      val positiveLiterals = a.positiveLiterals ++ b.positiveLiterals
  //      val negativeLiterals = a.negativeLiterals ++ b.negativeLiterals
  //
  //
  //
  //    }
  //
  //
  //    List<Literal> trPosLits = new ArrayList<Literal>();
  //    List<Literal> trNegLits = new ArrayList<Literal>();
  //    List<Literal> copyRPosLits = new ArrayList<Literal>();
  //    List<Literal> copyRNegLits = new ArrayList<Literal>();
  //
  //    for (int i = 0; i < 2; i++) {
  //      trPosLits.clear();
  //      trNegLits.clear();
  //
  //      if (i == 0) {
  //        // See if this clauses positives
  //        // unify with the other clauses
  //        // negatives
  //        trPosLits.addAll(this.positiveLiterals);
  //        trNegLits.addAll(othC.negativeLiterals);
  //      } else {
  //        // Try the other way round now
  //        trPosLits.addAll(othC.positiveLiterals);
  //        trNegLits.addAll(this.negativeLiterals);
  //      }
  //
  //      // Now check to see if they resolve
  //      Map<Variable, Term> copyRBindings = new LinkedHashMap<Variable, Term>();
  //      for (Literal pl : trPosLits) {
  //        for (Literal nl : trNegLits) {
  //          copyRBindings.clear();
  //          if (null != _unifier.unify(pl.getAtomicSentence(), nl
  //              .getAtomicSentence(), copyRBindings)) {
  //            copyRPosLits.clear();
  //            copyRNegLits.clear();
  //            boolean found = false;
  //            for (Literal l : allPosLits) {
  //              if (!found && pl.equals(l)) {
  //                found = true;
  //                continue;
  //              }
  //              copyRPosLits.add(_substVisitor.subst(copyRBindings,
  //                  l));
  //            }
  //            found = false;
  //            for (Literal l : allNegLits) {
  //              if (!found && nl.equals(l)) {
  //                found = true;
  //                continue;
  //              }
  //              copyRNegLits.add(_substVisitor.subst(copyRBindings,
  //                  l));
  //            }
  //            // Ensure the resolvents are standardized apart
  //            Map<Variable, Term> renameSubstitituon = _standardizeApart
  //                .standardizeApart(copyRPosLits, copyRNegLits,
  //                    _saIndexical);
  //            Clause c = new Clause(copyRPosLits, copyRNegLits);
  //            c.setProofStep(new ProofStepClauseBinaryResolvent(c,
  //                this, othC, copyRBindings, renameSubstitituon));
  //            if (isImmutable()) {
  //              c.setImmutable();
  //            }
  //            if (!isStandardizedApartCheckRequired()) {
  //              c.setStandardizedApartCheckNotRequired();
  //            }
  //            resolvents.add(c);
  //          }
  //        }
  //      }
  //    }
  //
  //    return resolvents;
  //
  //  }




}