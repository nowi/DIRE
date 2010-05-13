package core.resolution


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

class DALCResolver(env: {val inferenceRecorder: ClauseRecording; val recordProofSteps: Boolean; val standardizer: Standardizing; val selector: LiteralSelection; val literalComparator: LiteralComparison; val uniqueLiteralResolver: UniqueLiteralResolution}) extends BinaryResolution with Logging {
  val standardizer = env.standardizer
  implicit val selector = env.selector
  implicit val literalComparator = env.literalComparator
  val recordProofSteps = env.recordProofSteps
  val inferenceRecorder = env.inferenceRecorder
  implicit val uniqueLiteralResolver = env.uniqueLiteralResolver.apply _
  // some invariants , dalc resolver needs compatible selection and comperator

  // chache for maximal literalas
  val maxLitCache : MMap[FOLClause,Option[FOLNode]] = MMap()
  val uniqueRLitCache : MMap[FOLClause,Option[FOLNode]] = MMap()
  val selectedLitCache : MMap[FOLClause,Option[FOLNode]] = MMap()




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

  implicit def listFOLNode2ALCDClause(list: List[FOLNode]) = ALCDClause(list)


  private def applyWithIndexedStorage(a: FOLClause, clauses: ClauseStorage with UnifiableClauseRetrieval): Iterable[BinaryResolutionResult] = {


    val results: Iterable[BinaryResolutionResult] = uniqueRLitCache.getOrElseUpdate(a,a.uniqueResolvableLit) match {
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
                val mu = mgu(aPosS, bNegS.negate) match {
                  case Some(m) => m
                  case None => {
                    // this should not happe
                    error("Could not unfiy , but was retrieved as unifiable .. this cannot be")
                  }
                }

                log.ifDebug("MGU for Literal : %s and Literal %s is %s", aPosS, bNegS, mu)
                val S1: FOLClause = aS.rewrite(mu)
                val aLitS: FOLNode = aPosS.rewrite(mu)
                val S2: FOLClause = bS.rewrite(mu)
                val bLitS: FOLNode = bNegS.rewrite(mu)
                val resolved = ((S1 - aLitS) ++ (S2 - bLitS))
                if (resolved.isEmpty) {
                  log.ifInfo("Derived empty clause")
                }

                // reverse renamings
                if (!renamings.isEmpty)
                  SuccessfullResolution(ALCDClause(resolved).rewrite(renamings), ALCDClause(a).rewrite(renamings), ALCDClause(b).rewrite(renamings))
                else
                  SuccessfullResolution(resolved, a, b)


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
                val mu = mgu(bNegS.negate, aPosS) match {
                  case Some(m) => m
                  case None => {
                    // this should not happe
                    error("Could not unfiy , but was retrieved as unifiable .. this cannot be")
                  }
                }




                log.ifDebug("MGU for Literal : %s and Literal %s is %s", aPosS, bNegS, mu)
                val S1: FOLClause = aS.rewrite(mu)
                val aLitS: FOLNode = aPosS.rewrite(mu)
                val S2: FOLClause = bS.rewrite(mu)
                val bLitS: FOLNode = bNegS.rewrite(mu)
                val resolved = ((S1 - aLitS) ++ (S2 - bLitS))
                if (resolved.isEmpty) {
                  log.info("Derived empty clause")
                }

                // reverse renamings
                if (!renamings.isEmpty)
                  SuccessfullResolution(ALCDClause(resolved).rewrite(renamings), ALCDClause(a).rewrite(renamings), ALCDClause(b).rewrite(renamings))
                else
                  SuccessfullResolution(resolved, a, b)


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
