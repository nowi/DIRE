package core.reduction


import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import com.jteigen.scalatest.JUnit4Runner
import rewriting.{VariableRewriter}

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 15:09:50
 */
@RunWith(classOf[JUnit4Runner])
class StillmannSubsumerSpec extends SubsumptionSpec {
  override val subsumes = core.reduction.StillmannSubsumer
}