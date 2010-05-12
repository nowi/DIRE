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
abstract class ConferencePartitionedProvingSpec extends Spec with ShouldMatchers with Logging {
  def createProver: FOLProving

  describe("A FOLProver") {


    it("should locally saturate the module1 of partitioned conf example") {
      val prover: FOLProving = createProver

      val initialClauses = {
        // the curiosity killed the cat domain
        val partitioner = new ManualConfExamplePartitioner
        partitioner.partition(Nil)(0)

      }


      prover.saturate(initialClauses)._1 should equal(ProvingResult.COMPLETION)

    }


    it("should locally saturate the module2 of partitioned conf example") {
      val prover: FOLProving = createProver

      val initialClauses = {
        // the curiosity killed the cat domain
        val partitioner = new ManualConfExamplePartitioner
        partitioner.partition(Nil)(1)

      }


      prover.saturate(initialClauses)._1 should equal(ProvingResult.COMPLETION)

    }



    it("should locally saturate the module3 of partitioned conf example") {
      val prover: FOLProving = createProver

      val initialClauses = {
        // the curiosity killed the cat domain
        val partitioner = new ManualConfExamplePartitioner
        partitioner.partition(Nil)(2)

      }


      prover.saturate(initialClauses)._1 should equal(ProvingResult.COMPLETION)

    }

    it("should locally saturate the module4 of partitioned conf example") {
      val prover: FOLProving = createProver

      val initialClauses = {
        // the curiosity killed the cat domain
        val partitioner = new ManualConfExamplePartitioner
        partitioner.partition(Nil)(3)

      }


      prover.saturate(initialClauses)._1 should equal(ProvingResult.COMPLETION)

    }


    it("should locally saturate the module5 of partitioned conf example") {
      val prover: FOLProving = createProver

      val initialClauses = {
        // the curiosity killed the cat domain
        val partitioner = new ManualConfExamplePartitioner
        partitioner.partition(Nil)(4)

      }


      prover.saturate(initialClauses)._1 should equal(ProvingResult.COMPLETION)

    }


  }


}

