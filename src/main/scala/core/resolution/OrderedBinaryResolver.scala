package core.resolution

import caches.{MaxLitCache, SelectedLitCache, URLitCache}
import containers.ClauseStorage
import domain.fol.ast._
import domain.fol.functions.FOLAlgorithms._
import domain.fol.Substitution
import helpers.Logging
import ordering.LiteralComparison
import selection.LiteralSelection
import recording.ClauseRecording
import reduction.{Subsumption, ClauseCondenser, DuplicateLiteralDeleter}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 01.05.2010
 * Time: 19:27:14
 */

class OrderedBinaryResolver(env: {val standardizer: Standardizing;
  val subsumptionStrategy: Subsumption;
  val selector: LiteralSelection;
  val literalComparator: LiteralComparison; val uniqueRLitCache : URLitCache;
  val maxLitCache : MaxLitCache;val selectedLitCache : SelectedLitCache }) extends BinaryResolution with Logging {

  val standardizer = env.standardizer

  implicit val literalComparator = env.literalComparator
  implicit val selector = env.selector
  implicit val subsumptionChecker = env.subsumptionStrategy


  // get from global context and declare as implcit in this context
  implicit val maxLitCache : MaxLitCache = env.maxLitCache
  implicit val selectedLitCache : SelectedLitCache = env.selectedLitCache
  implicit val uniqueLitRCache : URLitCache  = env.uniqueRLitCache


  val condensation = ClauseCondenser
  val duplicateLiteralDeletion = DuplicateLiteralDeleter

  var iteration : Int = 1
  var totalCandidates : Int = 0

  override def apply(a: FOLClause, b: ClauseStorage) = {

    // check if we have index support

    val candidates  = b match {
      case indexedClauseStorage: core.containers.UnifiableClauseRetrieval => {
        // we have a structure that supports unifiables clause retrieval
        // get the unifiable clauses for each literal ( this is not a perfect filtering
        // because we have no unique literal to resolve upon
        // this changes in alcd where we can determine the unique resolvable literal
        // prior to substitution !

        a.literals.flatMap({lit : FOLNode => indexedClauseStorage.retrieveUnifiables(lit) ++ indexedClauseStorage.retrieveUnifiables(lit.negate) -- List(a)})
      }

      case _ => {
        // no index support
        log.warning("Resolving without index support !")
        // copy over all elements from the clause store -- expensive
        b.toList
      }


    }


    if(iteration % 50 == 0) {
      val avgCandidateSize = totalCandidates / iteration
      log.info("AVG candidate size : %s",avgCandidateSize )

    }
    iteration += 1
    totalCandidates += candidates.size

    // get the first successfullresolution result
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
    //val (aStand, bStand,_,_, renamings) = standardizer.standardizeApart(a, b)

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

  implicit def iterableFOLNode2StandardClause(iterable: Set[FOLNode]) = StandardClause(iterable)



  private def doResolve(a: Set[FOLNode], b: Set[FOLNode]) = {

    //either B is selected in D ∨ ¬B
    def condition2a(bNeg: Negation): Boolean = {
      b.selectedLits.contains(bNeg)
    }

    // nothing is selected in D ∨ ¬B and Bσ is maximal w.r.t. Dσ
    def condition2b(bNeg: Negation, mgu: Substitution): Boolean = {
      b.selectedLits.isEmpty && b.rewrite(mgu).maxLits.contains(bNeg.filler.rewrite(mgu))
    }

    // Aσ is strictly maximal with respect to Cσ
    def condition3(aPos: FOLNode, mgu: Substitution): Boolean = {
      a.rewrite(mgu).maxLits.contains(aPos.rewrite(mgu))
    }

    // nothing is selected in Cσ ∨ Aσ
    def condition4(aPos: FOLNode, mgu: Substitution): Boolean = {
      a.selectedLits.isEmpty
    }

    // we need to find 2 literals of opposite polarity
    // find the candidates

    val candidates = (for (aPos <- a.positiveLiterals;
                           bNeg <- b.negativeLiterals) yield (aPos, bNeg)).filter({case (t1, Negation(t2)) => t1.top == t2.top})

    // find the first pair of literals that unifies
    // TODO we could optimize the amount of rewritings here
    // TODO check if empty substitution causes problems
    val result = candidates.find({case (aPos, bNeg: Negation) => mgu(aPos, bNeg.filler).isDefined}) match {
      case Some((aPos : FOLNode, bNeg: Negation)) => {
        mgu(aPos, bNeg.filler) match {
          case Some(mu) if (OrderedResolution.isAppliable(a,aPos,b,bNeg,mu)  ) => {
            // orderer resolution possible , all conditions are met !
            log.debug("MGU for Literal : %s and Literal %s is %s", aPos, bNeg, mu)
            val S1  = a.map(_.rewrite(mu))
            val aLitS = aPos.rewrite(mu)
            val S2   = b.map(_.rewrite(mu))
            val bLitS = bNeg.rewrite(mu)
            val resolved  = ((S1 - aLitS) ++ (S2 - bLitS))
            if (resolved.isEmpty) {
              log.info("Derived empty clause")
            }
            SuccessfullResolution(StandardClause(resolved), a, b)


          }

          case Some(mu) => {
            // check which conditions failed
            val isMUEmpty = mu.isEmpty
            val passedC2 = (condition2a(bNeg) || condition2b(bNeg, mu))
            val passedC3 = condition3(aPos, mu)
            val passedC4 = condition4(aPos, mu)

            FailedResolution(a, Some(b))


          }

          case _ => {
            // no mgu , or not meeting ordering resolution restrictions
            FailedResolution(a, Some(b))
          }

        //          case Some(mu) => {
        //            // empty mgu, merge no substitution needed
        ////            val unduped = duplicateLiteralDeletion.apply(((a - aPos) ++ (b - bNeg)))
        ////            val condensed = condensation.apply(unduped)(subsumptionChecker)
        ////            condensed
        //            val resolved = ((a - aPos) ++ (b - bNeg))
        //            if (resolved.isEmpty) {
        //              log.info("Derived empty clause")
        //            }
        //            SuccessfullResolution(resolved, a, b)
        //          }

        }


      }

      case None => {
        // no candidate pair found
        FailedResolution(a, Some(b))

      }

    }
    result

    // TODO remove this !!!!!!!!!!!!!!
//    throw new NotImplementedException
//    SuccessfullResolution(Set(), a, b)

  }


}