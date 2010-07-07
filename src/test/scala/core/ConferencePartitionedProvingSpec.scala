package de.unima.dire.core

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 21:42:23
 */

import de.unima.dire.helpers.Logging
import de.unima.dire.partitioning.ManualConfExamplePartitioner


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

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

