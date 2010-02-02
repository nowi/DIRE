package core.reduction


import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import com.jteigen.scalatest.JUnit4Runner
import rewriting.{VariableRewriter, Substitutor}

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 15:09:50
 */
@RunWith(classOf[JUnit4Runner])
class StillmannSubsumerSpec extends SubsumptionSpec {
  val config = new Object {
    val standardizer = new Standardizer(this)
    val variableRewriter = new VariableRewriter()
    val unificator = new Unificator(this)
    val substitutor = new Substitutor(this)
  }

  override val subsumer = new StillmannSubsumer(config)
}