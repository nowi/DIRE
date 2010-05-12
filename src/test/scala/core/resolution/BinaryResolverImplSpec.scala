package core.resolution

/**
 * User: nowi
 * Date: 13.12.2009
 * Time: 14:38:22
 */
import com.jteigen.scalatest.JUnit4Runner

import config.{CuriosityKilledTheCatOrderedTheoremProvingConfig}
import org.junit.runner.RunWith

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import helpers.Logging
import recording.NaiveClauseRecorder
import reduction.StillmannSubsumer
import rewriting.VariableRewriter


@RunWith(classOf[JUnit4Runner])
class BinaryResolverImplSpec extends ResolutionSpec {
  val config = new Object {

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)
    lazy val inferenceRecorder = new NaiveClauseRecorder
    // settings
    val recordProofSteps = true
    val removeDuplicates = false
    val useLightesClauseHeuristic = true
    val usableBackSubsumption = true
    val forwardSubsumption = true
    val dropSeenClauses = false
    val useIndexing = true
    lazy val subsumptionStrategy = StillmannSubsumer

    

    val timeLimit: Long = 0

  }
  override val resolver = new BinaryResolver(config)
  override val positiveFactorer = PositiveFactorer

}