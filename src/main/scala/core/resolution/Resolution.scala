package core.resolution


import caches.{URLitCache, MaxLitCache, SelectedLitCache}
import containers.{UnifiableClauseRetrieval, MatchingClausesRetrieval, CNFClauseStore, ClauseStorage}
import domain.fol.ast._
import domain.fol.Substitution
import helpers.Logging
import ordering.LiteralComparison
import recording.ClauseRecording
import selection.LiteralSelection
import sun.reflect.generics.reflectiveObjects.NotImplementedException


/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 16:15:37
 */

trait Resolution extends Inference {
//  def resolve(a: ClauseStorage, b: ClauseStorage): ResolutionResult
  def apply(a: FOLClause, b: FOLClause): ResolutionResult
  def apply(a: FOLClause, b: ClauseStorage): Iterable[ResolutionResult]
}

trait BinaryResolution extends Resolution {
//  def resolve(a: ClauseStorage, b: ClauseStorage): ResolutionResult
  def apply(a: FOLClause, b: FOLClause): BinaryResolutionResult
  def apply(a: FOLClause, b: ClauseStorage): Iterable[BinaryResolutionResult]
}

trait OrderedResolution extends BinaryResolution {
  // TODO make apply parametrized with selection and ordering
}


object ALCDOrderedResolution extends Logging {
 implicit def listFOLNode2ALCDClause(list: Set[FOLNode]) = domain.fol.ast.ALCDClause(list)

  def isAppliable(sidePremise: FOLClause,aPos: FOLNode,mainPremise : FOLClause,bNeg: Negation)
                 (implicit selector: LiteralSelection,comperator: LiteralComparison,
                  uniqueRLitCache : URLitCache,maxLitCache : MaxLitCache,selectedLitCache : SelectedLitCache) : Boolean = {
    //B is selected in D ∨ ¬B ( main premise )
    def condition2a = {
      // TODO check the negative sign
      val selectedLits = mainPremise.selectedLits
      val r = selectedLits.contains(bNeg)
//      log.debug("either B is selected in D ∨ ¬B --> %s",r)
      r
    }

    // nothing is selected in D ∨ ¬B and Bσ is maximal w.r.t. Dσ
    def condition2b = {
      val mainPremiseSelectedLits = mainPremise.selectedLits
      val r = mainPremiseSelectedLits.isEmpty && mainPremise.maxLits.contains(bNeg)
//      log.debug("nothing is selected in D ∨ ¬B and Bσ is maximal w.r.t. Dσ --> %s",r)
      r
    }

    // Aσ is strictly maximal with respect to Cσ
    def condition3 = {
      val sidePremiseMaxLits = sidePremise.maxLits
      val r = sidePremiseMaxLits.contains(aPos)
//      log.debug("Aσ is strictly maximal with respect to Cσ --> %s",r)
      r
    }

    // nothing is selected in Cσ ∨ Aσ
    def condition4  = {
      val sidePremiseSelectedLits = sidePremise.selectedLits
      val r = sidePremiseSelectedLits.isEmpty
//      log.debug("nothing is selected in Cσ ∨ Aσ --> %s",r)
      r
    }




    //B is selected in D ∨ ¬B
    val c2a = condition2a

    // nothing is selected in D ∨ ¬B and Bσ is maximal w.r.t. Dσ
    val c2b = condition2b

    // Aσ is strictly maximal with respect to Cσ
    val c3 = condition3

    // nothing is selected in Cσ ∨ Aσ
    val c4 = condition4

   
    (condition2a || condition2b) && condition3 && condition4


  }


}

object OrderedResolution extends Logging {
 implicit def iterableFOLNode2ALCDClause(iterable: Set[FOLNode]) = StandardClause(iterable)

  def isAppliable(sidePremise: FOLClause,aPos: FOLNode,mainPremise : FOLClause,bNeg: Negation,mgu : Substitution)
                 (implicit selector: LiteralSelection,comperator: LiteralComparison,
                  uniqueRLitCache : URLitCache,maxLitCache : MaxLitCache,selectedLitCache : SelectedLitCache) : Boolean = {

    // Important :
    // Lemma 1 (Invariance of Maximality). If a set of ALC clauses is resolved applying RDL,
    // the maximal literal of a clause of type 1 or 2 is independent of unification.



    //B is selected in D ∨ ¬B ( main premise )
    def condition2a = {
      // TODO check the negative sign
      val selectedLits = mainPremise.selectedLits
      val r = selectedLits.contains(bNeg)
//      log.debug("either B is selected in D ∨ ¬B --> %s",r)
      r
    }

    // nothing is selected in D ∨ ¬B and Bσ is maximal w.r.t. Dσ
    def condition2b = {
      val mainPremiseSelectedLits = mainPremise.selectedLits


      val mainPremiseMaxLits = mainPremise.maxLits

      val BSigma = bNeg.rewrite(mgu)

      val r = mainPremiseSelectedLits.isEmpty && mainPremiseMaxLits.contains(BSigma)
//      log.debug("nothing is selected in D ∨ ¬B and Bσ is maximal w.r.t. Dσ --> %s",r)
      r
    }

    // Aσ is strictly maximal with respect to Cσ
    def condition3 = {
      val ASig =  aPos.rewrite(mgu)
      val sidePremiseS = sidePremise.rewrite(mgu)
      val sidePremiseSmaxLits = sidePremiseS.maxLits

      val r = sidePremiseSmaxLits.contains(ASig)
//      log.debug("Aσ is strictly maximal with respect to Cσ --> %s",r)
      r
    }

    // nothing is selected in Cσ ∨ Aσ
    def condition4  = {
      val sidePremiseS = sidePremise.rewrite(mgu)
      val sidePremiseSselectedLits = sidePremiseS.selectedLits
      val r = sidePremiseSselectedLits.isEmpty
//      log.debug("nothing is selected in Cσ ∨ Aσ --> %s",r)
      r
    }




    //B is selected in D ∨ ¬B
    val c2a = condition2a

    // nothing is selected in D ∨ ¬B and Bσ is maximal w.r.t. Dσ
    val c2b = condition2b

    // Aσ is strictly maximal with respect to Cσ
    val c3 = condition3

    // nothing is selected in Cσ ∨ Aσ
    val c4 = condition4


    (condition2a || condition2b) && condition3 && condition4


  }


}






