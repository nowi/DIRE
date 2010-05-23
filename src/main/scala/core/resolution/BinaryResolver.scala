package core.resolution


import containers.ClauseStorage
import domain.fol.ast._
import domain.fol.functions.FOLAlgorithms._
import domain.fol.Substitution
import helpers.Logging
import recording.ClauseRecording
import reduction.{Subsumption, ClauseCondenser, DuplicateLiteralDeleter}

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 17:08:18
 */

class BinaryResolver(env: {val standardizer: Standardizing; val subsumptionStrategy: Subsumption}) extends BinaryResolution with Logging {
  val standardizer = env.standardizer

  implicit def iterableFOLNode2StandardClause(iterable: Set[FOLNode]) = StandardClause(iterable)

  implicit val subsumptionChecker = env.subsumptionStrategy

  val condensation = ClauseCondenser
  val duplicateLiteralDeletion = DuplicateLiteralDeleter

  var iteration : Int = 1
  var totalCandidates : Int = 0

  override def apply(a: FOLClause, b: ClauseStorage) : Iterable[BinaryResolutionResult] = {

    // check if we have index support

    val candidates = b match {
      case indexedClauseStorage: core.containers.UnifiableClauseRetrieval => {
        // we have a structure that supports unifiables clause retrieval
        // get the unifiable clauses for each literal ( this is not a perfect filtering
        // because we have no unique literal to resolve upon
        // this changes in alcd where we can determine the unique resolvable literal
        // prior to substitution !
        a.literals.flatMap({lit : FOLNode => indexedClauseStorage.retrieveUnifiables(lit) ++ indexedClauseStorage.retrieveUnifiables(lit.negate) - a })
      }

      case _ => {
        // no index support
        log.warning("Resolving without index support !")
        b.toList
      }


    }

     if(iteration % 50 == 0) {
      val avgCandidateSize = totalCandidates / iteration
      log.info("AVG candidate size : %s",avgCandidateSize )

    }
    iteration += 1
    totalCandidates += candidates.size

    candidates.map(apply(a, _))



  }





  /**
   * Binary resolution always focuses on two clauses and one literal in each.
   * To admit a conclusion, the literals must be opposite in sign and alike in predicate,
   * and there must exist a mgu (substitution of terms for variables) to otherwise make them
   * identical. If a conclusion results, it is obtained by applying the mgu to the two
   * clauses excluding the two literals in focus, and taking the union of the transformed literals.
   */
  def apply(a: FOLClause, b: FOLClause): BinaryResolutionResult = {
    // TODO CEHCK THIS
    val (aStand, bStand,aSubst : Substitution,bSubst : Substitution,renamings) = standardizer.standardizeApart(a, b)

    log.debug("Resolving A : %s with B : %s", a, b)

    val conclusion = doResolve(aStand, bStand) match {
      case s: SuccessfullResolution => s
      case _ => {
        doResolve(bStand, aStand) match {
          case s: SuccessfullResolution => s
          case failure => failure
        }
      }
    }

    conclusion match {
      case SuccessfullResolution(result, parent1, parent2) if (!renamings.isEmpty) => {
        // rewrite
        SuccessfullResolution(ALCDClause(result).rewrite(renamings), ALCDClause(parent1).rewrite(renamings), ALCDClause(parent2).rewrite(renamings))

      }
      case _ => {
        conclusion
      }
    }

  }


  private def doResolve(a: FOLClause, b: FOLClause) = {

    val aLits = a.literals
    //    record.trace("Positive literals of standardized Clause %s are : %s", a, aLits)
    val bLits = b.literals
    //    record.trace("Negative literals of standardized Clause %s are : %s", b, bLits)

    // we need to find 2 literals of opposite polarity

    // find the candidates

    val candidates = (for (aPos <- a.positiveLiterals;
                           bNeg <- b.negativeLiterals) yield (aPos, bNeg)).filter({case (t1, Negation(t2)) => t1.top == t2.top})

    // find the first pair of literals that unifies

    val result = candidates.find({case (aPos, bNeg: Negation) => mgu(aPos, bNeg.filler).isDefined}) match {
      case Some((aPos, bNeg: Negation)) => {
        mgu(aPos, bNeg.filler) match {
          case Some(mu) if (!mu.isEmpty) => {
            // non empty mgu

            log.debug("MGU for Literal : %s and Literal %s is %s", aPos, bNeg, mu)
            val S1 = a.map(_.rewrite(mu))
            val aLitS = aPos.rewrite(mu)
            val S2 = b.map(_.rewrite(mu))
            val bLitS = bNeg.rewrite(mu)

//            val unduped = duplicateLiteralDeletion.apply(((S1 - aLitS) ++ (S2 - bLitS)))
//            val condensed = condensation.apply(unduped)(subsumptionChecker)
//            condensed

            val resolved = ((S1 - aLitS) ++ (S2 - bLitS))

            if (resolved.isEmpty) {
              log.info("Derived empty clause")
            }
            SuccessfullResolution(resolved, a, b)


          }

          case Some(mu) => {
            // empty mgu, merge no substitution needed
//            val unduped = duplicateLiteralDeletion.apply(((a - aPos) ++ (b - bNeg)))
//            val condensed = condensation.apply(unduped)(subsumptionChecker)
//            condensed
            val resolved = ((a - aPos) ++ (b - bNeg))
            if (resolved.isEmpty) {
              log.info("Derived empty clause")
            }
            SuccessfullResolution(resolved, a, b)
          }

          case None => {
            // no mgu
            FailedResolution(a, Some(b))
          }
        }


      }

      case None => {
        // no candidate pair found
        FailedResolution(a, Some(b))

      }

    }


    result

  }


}