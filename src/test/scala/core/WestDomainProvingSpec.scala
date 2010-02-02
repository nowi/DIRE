package core

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 12:26:36
 */

import com.jteigen.scalatest.JUnit4Runner

import config.WestDomain
import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


@RunWith(classOf[JUnit4Runner])
abstract class WestDomainProvingSpec extends Spec with ShouldMatchers {
  // create a proover
  val resolutionProover: FOLProving
  describe("WestDomainProving") {
    it("should prove that west is a criminal") {
      // prove that west is a criminal
      resolutionProover.prove(WestDomain.CriminalWestGoalClause) should equal(ProofFound()) // prove that west is a criminal
    }

    it("should prove that west is an american") {
      // prove that west is an american
      resolutionProover.prove(WestDomain.AmericanWestGoalClause) should equal(ProofFound())
    }


    it("should not refute that west is not criminal") {
      resolutionProover.prove(WestDomain.NotCriminalWestGoalClause) should not equal (ProofFound())

    }

  }
}