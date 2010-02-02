package partitioning

import com.jteigen.scalatest.JUnit4Runner

import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


/**
 * User: nowi
 * Date: 21.01.2010
 * Time: 19:56:30
 */
@RunWith(classOf[JUnit4Runner])
class ManualClauseStoragePartitionerSpec extends Spec with ShouldMatchers {
  describe("ManualClauseStoragePartitioner") {
    val filenames = List("input/conf/conf1.dire", "input/conf/conf2.dire", "input/conf/conf3.dire", "input/conf/conf4.dire")
    val partitioner = new ManualClauseStoragePartitioner(filenames)

    it("should return the conf example correctly from its seperate files") {
      // load the merged ontology
      // pass in empty clause Store , manual partitioning does not need a parent clause store
      val partitions = partitioner.partition(core.containers.CNFClauseStore())
      partitions.size should equal(5)

    }
  }
}