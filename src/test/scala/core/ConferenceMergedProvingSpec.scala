package de.unima.dire.core

/**
 * User: nowi
 * Date: 07.05.2010
 * Time: 16:44:47
 */


/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 21:42:23
 */

import de.unima.dire.core.config.{CuriosityDomain, WestDomain}
import de.unima.dire.domain.fol.ast._
import de.unima.dire.domain.fol.parsers.SPASSIntermediateFormatParser
import de.unima.dire.helpers.Logging
import de.unima.dire.partitioning.{ManualConfExampleMergerShared, ManualConfExampleMerger, ManualConfExamplePartitioner}
import de.unima.dire.core.rewriting.{VariableRewriter}


import java.io.File
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

abstract class ConferenceMergedProvingSpec extends Spec with ShouldMatchers with Logging {
  def createProver: FOLProving

  describe("A FOLProver") {
    it("should locally saturate the merged of partitioned conf example") {
      val prover: FOLProving = createProver
      val initialClauses = {
        // the curiosity killed the cat domain
        val partitioner = new ManualConfExampleMergerShared
        partitioner.partition(Nil).head

      }
      prover.saturate(initialClauses)._1 should equal(ProvingResult.COMPLETION)

    }

  }

}


