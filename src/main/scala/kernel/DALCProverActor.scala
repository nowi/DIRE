
package kernel
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import se.scalablesolutions.akka.dispatch.Dispatchers
import core._
import containers.{NonEmptyClauseStore, ClauseStorage, CNFClauseStore}
import domain.fol.ast.FOLClause
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Subject
import java.io.File
import core.reduction._
import core.rewriting.{Substitutor, VariableRewriter}
import ordering.{CustomConferencePartitionedPrecedence, LexicographicPrecedence, CustomSPASSModule1Precedence, ALCLPOComparator}
import ProvingState._
import ProvingResult._
import resolution.{DALCResolver, OrderedResolver}
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.util.Logging
import selection.{DALCRSelector, NegativeLiteralsSelection}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
/**
 * User: nowi
 * Date: 02.03.2010
 * Time: 12:34:38
 */

class DALCProverActor(val da: DispatchingActor) extends ProvingActor {
  override val coreProverConfig = new Object() {
    // empty initial clauses
    lazy val initialClauses = CNFClauseStore()

    lazy val tautologyDeleter = new TautologyDeleter()
    lazy val variableRewriter = new VariableRewriter()
    lazy val subsumptionDeleter = new SubsumptionDeleter(this)
    lazy val standardizer = new Standardizer(this)
    lazy val unificator = new Unificator(this)
    lazy val substitutor = new Substitutor(this)
    lazy val factorizer = new OrderedFactorizer(this)
    lazy val resolver = new DALCResolver(this)
    lazy val subsumptionStrategy = new StillmannSubsumer(this)

    // ordered resolution needs comparator and selection too
    lazy val precedence = new CustomConferencePartitionedPrecedence
    lazy val literalComparator = new ALCLPOComparator(this)
    lazy val selector = new DALCRSelector

    // settings
    val recordProofSteps = true
    val removeDuplicates = false
    val useLightesClauseHeuristic = true
    val usableBackSubsumption = false
    val forwardSubsumption = true
    val dropSeenClauses = false
    val useIndexing = true

    // set a time limit
    val timeLimit: Long = (20 * 1000)  // 10 sec
  }

  override val dispatcherActor = da

  override val prover = new ResolutionProover1(coreProverConfig)

  log.info("Core prooving subsystem started up with kernel : %s", prover)
}