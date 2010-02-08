package core.resolution


import containers.{MatchingClausesRetrieval, CNFClauseStore, ClauseStorage}
import domain.fol.ast._
import helpers.Logging
import ordering.LiteralComparison

import reduction.Factoring
import rewriting.Substitution
import selection.LiteralSelection


/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 16:15:37
 */

trait Resolution extends InferenceRecording {
  def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  def resolve(a: FOLClause, b: ClauseStorage): ClauseStorage


  def resolve(a: FOLClause, b: FOLClause): Set[FOLClause]

}



class BinaryResolver(env: {val unificator: Unify; val factorizer: Factoring; val standardizer: Standardizing; val substitutor: Substitution}) extends Resolution with Logging{
  val unificator = env.unificator
  val factorizer = env.factorizer
  val standardizer = env.standardizer
  val substitutor = env.substitutor


  def resolve(a: FOLClause, b: MatchingClausesRetrieval) = null


  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Resolving %s with %s", a, b)
    // resolve

    (for (clause1 <- a;
          clause2 <- b;
          if (clause1 != clause2);
          resolvent = CNFClauseStore(resolve(clause1, clause2)))
    yield resolvent).reduceLeft(_ ::: _)


  }

  override def resolve(a: FOLClause, b: ClauseStorage): ClauseStorage = {
    log.trace("Resolving %s with %s", a, b)
    // resolve

    (for (clause2 <- b;
          if (a != clause2);
          resolvent = CNFClauseStore(resolve(a, clause2)))
    yield resolvent).reduceLeft(_ ::: _)


  }


  /**
   * Binary resolution always focuses on two clauses and one literal in each.
   * To admit a conclusion, the literals must be opposite in sign and alike in predicate,
   * and there must exist a unifier (substitution of terms for variables) to otherwise make them
   * identical. If a conclusion results, it is obtained by applying the unifier to the two
   * clauses excluding the two literals in focus, and taking the union of the transformed literals.
   */
  override def resolve(a: FOLClause, b: FOLClause): Set[FOLClause] = {
    log.trace("Resolving the Clauses %s,%s", a, b)
    // standardize apart the clauses
    val (aStand, bStand) = standardizer.standardizeApart(a, b)

    // Apos , Bneg

    val conclusions = doResolve(aStand, bStand) ++
            doResolve(bStand, aStand)

    if (!conclusions.isEmpty) {
      log.info("%s + %s --> %s", (a, b, conclusions))
    } else {
      log.trace("RESOLVED NOTHING from clauses %s and %s !", a, b)
    }
    conclusions

  }


  private def doResolve(a: FOLClause, b: FOLClause): Set[FOLClause] = {

    val aLits = a.positiveLiterals
    //    log.trace("Positive literals of standardized Clause %s are : %s", a, aLits)
    val bLits = b.negativeLiterals
    //    log.trace("Negative literals of standardized Clause %s are : %s", b, bLits)

    val conclusions: Set[FOLClause] = (for (aPos <- aLits;
                                            bNeg <- bLits;
                                            mgu = unificator.unify(aPos, bNeg);
                                            if (mgu != None))
    yield mgu match {
        case Some(x) => {
          // we have a mgu
          // apply it to the two clauses excluding the 2 focused
          log.trace("MGU for Literal : %s and Literal %s is %s", Array(aPos, bNeg, mgu))
          //            Let S_1 and S_2 be two clauses with no variables in common, let S_1 contain a positive literal L_1, S_2 contain a negative literal L_2, and let eta be the most general unifier of L_1 and L_2. Then
          //(S_1eta-L_1eta) union (S_2eta-L_2eta)

          val sC = (clause: FOLClause) => substitutor.substitute(mgu, clause)
          val sN = (node: FOLNode) => substitutor.substitute(mgu, node)


          val S1 = sC(a)
          val aPosS = sN(aPos)
          val S2 = sC(b)
          val bNegS = sN(bNeg)

          log.warning("a : %s", S1)
          log.warning("aPos : %s", aPosS)
          log.warning("S2 : %s", S2)
          log.warning("bNegS : %s", bNegS)

          val resolvent: FOLClause = (S1 - aPosS) ++ (S2 - bNegS)



          if (resolvent.isEmpty) {
            log.debug("EMPTY CLAUSE RESOLVED FROM %s and %s", a, b)
            EmptyClause()
          } else {
            resolvent
          }

        }
        case None => {
          log.trace("Could not resolve Literals %s,%s", aPos, bNeg)
          domain.fol.ast.StandardClause()
        }
      })

    conclusions

  }


}


class OrderedResolver(env: {val useIndexing: Boolean; val recordProofSteps: Boolean; val unificator: Unify; val factorizer: Factoring; val standardizer: Standardizing; val substitutor: Substitution; val selector: LiteralSelection; val literalComparator: LiteralComparison}) extends Resolution with Logging {
  val unificator = env.unificator
  val factorizer = env.factorizer
  val standardizer = env.standardizer
  val substitutor = env.substitutor
  val selector = env.selector
  val literalComparator = env.literalComparator
  val recordProofSteps = env.recordProofSteps
  val useIndexing = env.useIndexing

  // ordered resolver needs a clause comparator

  // literal selector




  override def resolve(a: FOLClause, b: ClauseStorage) = {
    b match {
      case store: MatchingClausesRetrieval if (useIndexing) => resolveWithMatchingIndex(a, store)
      case _ => resolveWithoutIndex(a, b)
    }

    


  }

  private def resolveWithoutIndex(a: FOLClause, b: ClauseStorage) = {
    log.trace("Resolving %s with %s", a, b)
    // resolve
    val resolvents = (for (clause2 <- b;
                           if (a != clause2);
                           resolvent = CNFClauseStore(resolve(a, clause2)))
    yield resolvent)

    resolvents match {
      case x if (x.size == 0) => CNFClauseStore()
      case x => x.reduceLeft(_ ::: _)

    }


  }

  private def resolveWithMatchingIndex(a: FOLClause, b: MatchingClausesRetrieval): ClauseStorage = {
    log.trace("Resolving with MatchingIndexSupport %s with %s", a, b)


    // get the matching claueses , substruct the query clause a
    val matchingClauses = a.literals.map({lit: FOLNode => b.getMatchingClauses(lit).getOrElse(Set()) ++ b.getMatchingClauses(Negation(lit)).getOrElse(Set())}).reduceLeft(_ ++ _) - a

    matchingClauses match {
      case clauses: Set[FOLClause] if (clauses.isEmpty) => CNFClauseStore()
      case clauses: Set[FOLClause] => {
        val resolvents = (for (clause2 <- clauses;
                               if (a != clause2);
                               resolvent = CNFClauseStore(resolve(a, clause2)))
        yield resolvent)

        resolvents match {
          case x if (x.size == 0) => CNFClauseStore()
          case x => x.reduceLeft(_ ::: _)

        }

      }

    }


  }

  //  def resolveWithIndex(a: FOLClause, b: ImPerfectFilteringFOLClauseIndex) = {
  //    log.trace("Resolving %s with %s", a, b)
  //    // resolve
  //    (for (clause2 <- b;
  //                            if (a != clause2);
  //                            resolvent = CNFClauseStore(resolve(a, clause2)))
  //    yield resolvent).reduceLeft(_ ::: _)
  //
  //  }




  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Resolving %s with %s", a, b)
    // resolve


    (for (clause1 <- a;
          clause2 <- b;
          if (clause1 != clause2);
          resolvent = CNFClauseStore(resolve(clause1, clause2)))
    yield resolvent).reduceLeft(_ ::: _)

  }


  /**
   * Binary resolution always focuses on two clauses and one literal in each.
   * To admit a conclusion, the literals must be opposite in sign and alike in predicate,
   * and there must exist a unifier (substitution of terms for variables) to otherwise make them
   * identical. If a conclusion results, it is obtained by applying the unifier to the two
   * clauses excluding the two literals in focus, and taking the union of the transformed literals.
   */
  override def resolve(a: FOLClause, b: FOLClause): Set[FOLClause] = {
    log.trace("Resolving the Clauses %s,%s", a, b)

    val (aStand, bStand) = standardizer.standardizeApart(a, b)

    val conclusions: Set[FOLClause] =
    (doResolve(aStand, bStand)
            ++ doResolve(bStand, aStand)
            )
            .filter({
      _ match {
        case Some(c) => true
        case _ => false
      }
    })
            .map({_.get})

    if (!conclusions.isEmpty)
      log.debug("%s + %s --> %s" format (a, b, conclusions))


    conclusions


  }


  private def doResolve(a: FOLClause, b: FOLClause): Set[Option[FOLClause]] = {
    val aLits = a.positiveLiterals
    //    log.trace("Positive literals of standardized Clause %s are : %s", a, aLits)
    val bLits = b.negativeLiterals
    //    log.trace("Negative literals of standardized Clause %s are : %s", b, bLits)

    val conclusions: Set[Option[FOLClause]] = (for (
      aPos <- aLits;
      bNeg <- bLits;
      (aPosStand, bNegStand) = standardizer.standardizeApart(aPos, bNeg);
      mgu = unificator.unify(aPos, bNeg))
    yield mgu match {
        case Some(x) => {
          // we have a mgu

          // check the ordererd resolution preconditions
          if (checkSidePremise(a, aPos, mgu) && checkMainPremise(b, bNeg, mgu)) {
            // apply it to the two clauses excluding the 2 focused
            log.debug("Resolving on literal %s", bNeg)
            log.debug("MGU for Literal : %s and Literal %s is %s" format (aPos, bNeg, mgu))
            //            Let S_1 and S_2 be two clauses with no variables in common, let S_1 contain a positive literal L_1, S_2 contain a negative literal L_2, and let eta be the most general unifier of L_1 and L_2. Then
            //(S_1eta-L_1eta) union (S_2eta-L_2eta)
            // substition function
            // s(lit) <=> σ
            val sC = (clause: FOLClause) => substitutor.substitute(mgu, clause)
            val sN = (node: FOLNode) => substitutor.substitute(mgu, node)


            val S1 = sC(a)
            val aPosS = sN(aPos)
            val S2 = sC(b)
            val bNegS = sN(bNeg)

            log.debug("a : %s", S1)
            log.debug("aPos : %s", aPosS)
            log.debug("S2 : %s", S2)
            log.debug("bNegS : %s", bNegS)

            val resolvent: FOLClause = (S1 - aPosS) ++ (S2 - bNegS)



            if (resolvent.isEmpty) {
              log.debug("EMPTY CLAUSE RESOLVED FROM %s and %s", a, b)
              Some(EmptyClause())
            } else {
              log.debug("Resolved %s , from Literals %s,%s", List(resolvent, aPos, bNeg))
              // do backsubstitution

              if (recordProofSteps) {
                inferenceLog += (resolvent -> (a, b))
              }


              Some(resolvent)
            }

          } else {
            log.debug("Not satisfying ordering constaraints , not resolving Literals %s,%s", aPos, bNeg)
            None
          }

        }
        case None => {
          log.debug("Could not resolve Literals %s,%s", aPos, bNeg)
          None
        }
      })

    conclusions

  }

  // check preconditions for ordered resolution
  private def checkSidePremise(premise: FOLClause, literal: FOLNode, mgu: Option[Map[Variable, FOLNode]]): Boolean = {
    // 2.) the side premise contains no selected atoms
    //1.) we only resolve on the maximal litaral A
    // A has to be maximal in the side premise a V a AND the side premise contains
    // no seleted (negative) atoms



    if (selector.selectedLiterals(premise).isEmpty) {
      // get maximal lit of a
      val compare = literalComparator.compare(_, _)
      // substition function
      val s = (node: FOLNode) => substitutor.substitute(mgu, node)

      // and no lit in premise is greater then literal
      //val maximumLit = premise.literals.find({max: FOLNode => premise.literals.forall({lit: FOLNode => compare(s(max), s(lit)) == Some(1)})})
      val greaterThanLiteral = premise.literals.find({
        lit: FOLNode => (compare(s(lit), s(literal)) match {
          case Some(1) => true
          case _ => false
        })
      })


      greaterThanLiteral match {
        case None => {
          // we have found nothing greater
          log.debug("We have NOT found a greater Lit : %s is Striclty Maximal in sidepremise: %s", literal, premise)
          true
        }
        case _ => {
          log.debug("We have found a greater/greaterequal Lit : %s therefore %s is not maximal", greaterThanLiteral, literal)
          false
        }
      }


    }
    else {
      log.debug("Selected Literals : %s in sidePremise : %s were not empty ! ", selector.selectedLiterals(premise), premise)
      false
    }


  }

  private def checkMainPremise(premise: FOLClause, literal: FOLNode, mgu: Option[Map[Variable, FOLNode]]): Boolean = {
    // either B is selected in D ∨¬B or else nothing is selected in D ∨¬B and Bσ is maximal
    // w.r.t. Dσ

    // lit has to be a NegativeFOLLiteral
    //    assert((literal match {
    //      case NegativeFOLLiteral(x) => true
    //      case _ => false
    //    }), "Literal passed to the mainpremise check must be negative")


    if (selector.isSelected(literal, premise)) {
      true
    } else if (selector.selectedLiterals(premise).isEmpty) {
      // get maximal lit of a
      val compare = literalComparator.compare(_, _)
      // substition function
      val s = (node: FOLNode) => substitutor.substitute(mgu, node)

      val greaterThanLiteral = premise.literals.find({
        lit: FOLNode => (compare(s(lit), s(literal)) match {
          case Some(1) => true
          case _ => false
        })
      })

      greaterThanLiteral match {
        case None => {
          // we have found nothing greater
          log.debug("We have NOT found a greater Lit : %s is Striclty Maximal in mainpremise: %s", literal, premise)
          true
        }
        case _ => {
          log.debug("We have found a greater/greaterequal Lit : %s therefore %s is not maximal", greaterThanLiteral, literal)
          false
        }
      }


    } else {
      false
    }


  }
}





class GeneralResolution(env: {val unificator: Unify}) extends Resolution with Logging {
  val unificator = env.unificator


  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Resolving %s with %s", a, b)
    a
  }

  def resolve(a: FOLClause, b: MatchingClausesRetrieval) = null

  /**
   * Deﬁnition 8.5.2 Given two clauses A and B, a clause C is a resolvent of
   * A and B iﬀ the following holds:
   *
   * (i) There is a subset A′ =                                                  { A1 , ..., Am } ⊆ A of literals all of the same sign,
   * a subset B′ =                                                  { B1 , ..., Bn } ⊆ B of literals all of the opposite sign of the set A′ ,
   * and a separating pair of substitutions (ρ, ρ′ ) such that the set |ρ(A′ ) ∪ ρ′ (B′ )|
   * is uniﬁable;
   *
   * (ii) For some most general uniﬁer σ of the set |ρ(A′ ) ∪ ρ′ (B′ )|, we have
   * C = σ(ρ(A − A′ ) ∪ ρ′ (B − B′ )).
   */


  def resolve(a: FOLClause, b: ClauseStorage) = null

  override def resolve(a: FOLClause, b: FOLClause): Set[FOLClause] = {
    Set(a)
  }


}