package de.unima.dire.core

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 21:42:23
 */

import de.unima.dire.core.config.{CuriosityDomain, WestDomain}
import de.unima.dire.domain.fol.ast._
import de.unima.dire.domain.fol.parsers.SPASSIntermediateFormatParser
import de.unima.dire.helpers.Logging
import de.unima.dire.partitioning.ManualConfExamplePartitioner
import de.unima.dire.core.rewriting.{VariableRewriter}

import java.io.File


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

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




