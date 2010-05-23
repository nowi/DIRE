package core

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 12:26:36
 */

import com.jteigen.scalatest.JUnit4Runner

import config.CuriosityDomain
import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import ProvingResult._

                              
@RunWith(classOf[JUnit4Runner])
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