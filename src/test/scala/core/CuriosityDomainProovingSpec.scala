package de.unima.dire.core

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 12:26:36
 */


import de.unima.dire.core.config.CuriosityDomain
import de.unima.dire.core.containers.{CNFClauseStore}
import de.unima.dire.core.ProvingResult._

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

                              
abstract class CuriosityDomainProovingSpec extends Spec with ShouldMatchers {
  // create a proover
  val resolutionProover: FOLProving
  describe("CuriosityDomainProoving") {
    it("should entail that curiosity killed the cat") {
      // create a proover

      resolutionProover.saturate(CuriosityDomain.initialClauses :::  List(CuriosityDomain.curiosityKilledTunaGoalClause)) match {
        case (PROOF,_,_,_,_) => assert(true)
        case _ => assert(false)
      }


    }

  }
}