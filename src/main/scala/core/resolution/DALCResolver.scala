package core.resolution


import caches.{SelectedLitCache, MaxLitCache, URLitCache}
import collection.immutable.EmptyMap
import containers.{UnifiableClauseRetrieval, MatchingClausesRetrieval, CNFClauseStore, ClauseStorage}
import domain.fol.ast._
import domain.fol.functions.FOLAlgorithms
import domain.fol.Substitution
import helpers.Logging
import ordering.{ALCLPOComparator, LiteralComparison}
import recording.ClauseRecording
import selection.{DALCRSelector, LiteralSelection}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import FOLAlgorithms._
import scala.collection.mutable.{Map => MMap}

class DALCResolver(env: { val standardizer: Standardizing;
  val selector: LiteralSelection; val literalComparator: LiteralComparison;
  val uniqueLiteralResolver: Option[UniqueLiteralResolution];
  val uniqueRLitCache : URLitCache;
  val maxLitCache : MaxLitCache;val selectedLitCache : SelectedLitCache }) extends BinaryResolution with Logging {

  val standardizer = env.standardizer
  implicit val selector = env.selector
  implicit val literalComparator = env.literalComparator

  implicit val uniqueLiteralResolver = env.uniqueLiteralResolver
  // some invariants , dalc resolver needs compatible selection and comperator


  // get from global context and declare as implcit in this context
  implicit val maxLitCache : MaxLitCache = env.maxLitCache
  implicit val selectedLitCache : SelectedLitCache = env.selectedLitCache
  implicit val uniqueLitRCache : URLitCache  = env.uniqueRLitCache




  //require(literalComparator.isInstanceOf[ALCLPOComparator])

  //require(selector.isInstanceOf[ALCRSelector])
  var iteration: Int = 1
  var totalCandidates: Int = 0

  override def apply(a: FOLClause, b: ClauseStorage): Iterable[BinaryResolutionResult] = {
    b match {

      case indexedClauseStorage: ClauseStorage with UnifiableClauseRetrieval => {
        // we have a structure that supports unifiables clause retrieval
        // get the unifiable clauses for each literal ( this is not a perfect filtering
        // because we have no unique literal to resolve upon
        // this changes in alcd where we can determine the unique resolvable literal
        // prior to substitution !
        // get the UNRL
        applyWithIndexedStorage(a, indexedClauseStorage)
      }
      case _ => {
        throw new NotImplementedException
      }

    }


  }

  implicit def iterableFOLNode2ALCDClause(iterable: Set[FOLNode]) = ALCDClause(iterable)


  private def applyWithIndexedStorage(a: FOLClause, clauses: ClauseStorage with UnifiableClauseRetrieval): Iterable[BinaryResolutionResult] = {


    val results: Iterable[BinaryResolutionResult] = a.uniqueResolvableLit(uniqueLiteralResolver,uniqueLitRCache) match {
      case (Some(aUrLit)) => {



        // resolve on unique literal
        // check if clause a is main or sidepremise
        aUrLit match {
          case aPos if (aUrLit.positive) => {
            // a is side premise - resolveable literal is positive // get negative candidate clauses


            val mainPremises = clauses.retrieveUnifiablesFull(aPos.negate)


            // resolve a with all candidates
            for (mainPremise <- mainPremises) yield {

              val (b: FOLClause, bNeg: Negation) = mainPremise

              // check if we can apply ordered resolution
              if (ALCDOrderedResolution.isAppliable(a, aPos, b, bNeg)) {

                // standardize apart
                // TODO optimize this
                // rename the variables in clause a such that there are no collisions


                // standardize apart
                // TODO optimize this
                // rename the variables in clause a such that there are no collisions

                val (aS, bS, aSubst: Substitution, bSubst: Substitution, renamings) = standardizer.standardizeApart(a, b)

                val (aPosS: FOLNode, bNegS: Negation) = (aPos.rewrite(aSubst), bNeg.rewrite(bSubst))

                // get the unifier
                mgu(aPosS, bNegS.negate) match {
                  case Some(mu) => {
                    log.ifDebug("MGU for Literal : %s and Literal %s is %s", aPosS, bNegS, mu)
                    val S1 = aS.rewrite(mu)
                    val aLitS = aPosS.rewrite(mu)
                    val S2 = bS.rewrite(mu)
                    val bLitS = bNegS.rewrite(mu)
                    val resolved = ((S1 - aLitS) ++ (S2 - bLitS))
                    if (resolved.isEmpty) {
                      log.ifInfo("Derived empty clause")
                    }

                    // reverse renamings
                    if (!renamings.isEmpty)
                      SuccessfullResolution(ALCDClause(resolved).rewrite(renamings), ALCDClause(a).rewrite(renamings), ALCDClause(b).rewrite(renamings))
                    else
                      SuccessfullResolution(resolved, a, b)


                  }
                  case None => {
                    FailedResolution(a, Some(b))
                    // this should not happe
                    //error("Could not unfiy , but was retrieved as unifiable .. this should not happen with perfect filtering")
                  }
                }


              } else {
                FailedResolution(a, Some(b))

              }


            }

          }

          case bNeg: Negation => {
            // a is main premise == hence a becomes b in this context
            val b = a
            val sidePremises = clauses.retrieveUnifiablesFull(bNeg.negate)
            // resolve a with all candidates
            for (sidePremise <- sidePremises) yield {
              val (a, aPos) = sidePremise
              // standardize apart
              // TODO optimize this
              // rename the variables in clause a such that there are no collisions


              // check if we can apply ordered resolution
              if (ALCDOrderedResolution.isAppliable(a, aPos, b, bNeg)) {

                val (aS, bS, aSubst: Substitution, bSubst: Substitution, renamings) = standardizer.standardizeApart(a, b)

                val (aPosS: FOLNode, bNegS: Negation) = (aPos.rewrite(aSubst), bNeg.rewrite(bSubst))

                // get thte unifier
                // get the unifier
                mgu(bNegS.negate, aPosS) match {
                  case Some(mu) => {
                    log.ifDebug("MGU for Literal : %s and Literal %s is %s", aPosS, bNegS, mu)
                    val S1  = aS.rewrite(mu)
                    val aLitS  = aPosS.rewrite(mu)
                    val S2  = bS.rewrite(mu)
                    val bLitS  = bNegS.rewrite(mu)
                    val resolved  = ((S1 - aLitS) ++ (S2 - bLitS))
                    if (resolved.isEmpty) {
                      log.info("Derived empty clause")
                    }

                    // reverse renamings
                    if (!renamings.isEmpty)
                      SuccessfullResolution(ALCDClause(resolved).rewrite(renamings), ALCDClause(a).rewrite(renamings), ALCDClause(b).rewrite(renamings))
                    else
                      SuccessfullResolution(resolved, a, b)


                  }
                  case None => {
                    FailedResolution(a, Some(b))
                    // this should not happen with perfect filtering
                    //error("Could not unfiy , but was retrieved as unifiable .. this cannot be")
                  }
                }


              } else {
                FailedResolution(a, Some(b))

              }

            }
          }
        }
      }

      case None => {
        log.debug("Given clause %s .... NO URLit", a)
        // no unique resolvable Ltieral for clause a
        // a is no premise for resolution
        // only positive factorign might be possible
        List(FailedResolution(a, None))

      }
    }

    val successes = results.filter {_.isInstanceOf[SuccessfullResolution]}

    successes
  }


  def apply(a: FOLClause, b: FOLClause) = throw new NotImplementedException
}
