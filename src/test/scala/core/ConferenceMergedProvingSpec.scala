package core

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
import com.jteigen.scalatest.JUnit4Runner

import config.{CuriosityDomain, WestDomain}
import domain.fol.ast._
import domain.fol.parsers.SPASSIntermediateFormatParser
import java.io.File
import helpers.Logging
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import partitioning.{ManualConfExampleMergerShared, ManualConfExampleMerger, ManualConfExamplePartitioner}
import rewriting.{VariableRewriter}

@RunWith(classOf[JUnit4Runner])
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


