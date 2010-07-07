package de.unima.dire.core.resolution

/**
 * User: nowi
 * Date: 13.12.2009
 * Time: 14:38:22
 */



import de.unima.dire.domain.fol.ast._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import de.unima.dire.helpers.Logging
import de.unima.dire.recording.NaiveClauseRecorder
import de.unima.dire.core.reduction.StillmannSubsumer
import de.unima.dire.core.rewriting.VariableRewriter
import de.unima.dire.core.config.CuriosityKilledTheCatOrderedTheoremProvingConfig
import de.unima.dire.core.Standardizer


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