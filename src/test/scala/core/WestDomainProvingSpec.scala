package core

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 12:26:36
 */

import com.jteigen.scalatest.JUnit4Runner

import config.{WestDomain}
import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

import ProvingResult._

@RunWith(classOf[JUnit4Runner])
abstract class WestDomainProvingSpec extends Spec with ShouldMatchers {
  // create a proover
  val resolutionProover: FOLProving
  describe("WestDomainProving") {
    it("should entail that west is a criminal") {
      // entail that west is a criminal
      resolutionProover.saturate(WestDomain.initialClauses  ::: List(WestDomain.CriminalWestGoalClause)) match {
        case (PROOF,_,_,_,_) => assert(true)
        case _ => assert(false)
      }
    }

    it("should entail that west is an american") {
      // entail that west is an american
      resolutionProover.saturate(WestDomain.initialClauses  ::: List(WestDomain.AmericanWestGoalClause)) match {
        case (PROOF,_,_,_,_) => assert(true)
        case _ => assert(false)
      }
    }


    it("should not refute that west is not criminal") {
      resolutionProover.saturate(WestDomain.initialClauses  ::: List(WestDomain.NotCriminalWestGoalClause))  match {
        case (PROOF,_,_,_,_) => assert(false)
        case _ => assert(true)
      }

    }

  }
}