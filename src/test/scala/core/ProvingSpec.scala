package core

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 21:42:23
 */
import com.jteigen.scalatest.JUnit4Runner

import config.{CuriosityDomain, WestDomain}
import domain.fol.ast._
import domain.fol.parsers.SPASSIntermediateFormatParser
import java.io.File
import helpers.Logging
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import partitioning.ManualConfExamplePartitioner
import rewriting.{VariableRewriter}

@RunWith(classOf[JUnit4Runner])
abstract class ProvingSpec extends Spec with ShouldMatchers with Logging {
  def createProver: FOLProving

  describe("A FOLProver") {
    it("should refute the west example") {
      val prover: FOLProving = createProver
      prover.saturate(WestDomain.initialClauses ::: List(WestDomain.CriminalWestGoalClause))._1 should equal(ProvingResult.PROOF)
    }

    it("should complete the west example") {
      val prover: FOLProving = createProver
      prover.saturate(WestDomain.initialClauses)._1 should equal(ProvingResult.COMPLETION)
    }

    it("should refute the curiosity killed the cat example") {
      val prover: FOLProving = createProver
      prover.saturate(CuriosityDomain.initialClauses ::: List(CuriosityDomain.curiosityKilledTunaGoalClause))._1 should equal(ProvingResult.PROOF)

    }
    ////
    it("should complete the curiosity killed the cat example") {
      val prover: FOLProving = createProver
      prover.saturate(CuriosityDomain.initialClauses)._1 should equal(ProvingResult.COMPLETION)

    }

   

  }


}




