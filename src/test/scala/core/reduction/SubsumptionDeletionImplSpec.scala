package core.reduction

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 16:11:02
 */
import config.TheoremProvingConfig1
import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import com.jteigen.scalatest.JUnit4Runner

@RunWith(classOf[JUnit4Runner])
class SubsumptionDeletionImplSpec extends SubsumptionDeletionSpec {
  override val subsumptionDeleter = new SubsumptionDeleter(TheoremProvingConfig1)
}