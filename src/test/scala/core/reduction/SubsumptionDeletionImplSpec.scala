package core.reduction

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 16:11:02
 */
import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import com.jteigen.scalatest.JUnit4Runner
import rewriting.{VariableRewriter, Substitutor}

@RunWith(classOf[JUnit4Runner])
class SubsumptionDeletionImplSpec extends SubsumptionDeletionSpec {
  val config = new Object {
    val standardizer = new Standardizer(this)
    val variableRewriter = new VariableRewriter()
    val unificator = new Unificator(this)
    val substitutor = new Substitutor(this)
    val subsumptionStrategy = new StillmannSubsumer(this)

  }
  override val subsumptionDeleter = new SubsumptionDeleter(config)
}