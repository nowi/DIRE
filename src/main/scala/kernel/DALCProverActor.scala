package kernel

import dispatching.{ToVoidDispatchingActor, DALCDispatcherActor}
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import recording.NaiveClauseRecorder
import se.scalablesolutions.akka.dispatch.Dispatchers
import core._
import config.DALCConfig
import containers.heuristics.{LightestClauseHeuristicStorage, ListBufferStorage}
import containers.{MutableClauseStore, SForrestIndex, ClauseStorage, CNFClauseStore}
import domain.fol.ast.FOLClause
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Subject
import java.io.File
import core.reduction._
import core.rewriting.{VariableRewriter}
import ordering.{CustomConferencePartitionedPrecedence, CustomSPASSModule1Precedence, ALCLPOComparator}
import ProvingState._
import ProvingResult._
import resolution.{DALCUniqueLiteralResolver, DALCResolver}
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.util.Logging
import selection.{DALCRSelector, NegativeLiteralsSelection}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 02.03.2010
 * Time: 12:34:38
 */

trait Neo4JLoggingActorFactory {
  this: ReasoningActor =>
  // create the dispatcher actor
  val derivationsLoggingActor = new Neo4JLoggingActor(DALCConfig)

  val reductionsLoggingActor = new Neo4JLoggingActor(DALCConfig)

}

trait DALCDispatcherActorFactory {
  this: ReasoningActor =>
  // create the dispatcher actor
  val dispatcherActor  = new DALCDispatcherActor(DALCConfig)
}

trait ToVoidDispatcherActorFactory {
  this: ReasoningActor =>
  // create the dispatcher actor
  val dispatcherActor = new ToVoidDispatchingActor
}


trait DALCProverActorFactory {
  this: ReasoningActor =>
  // configure the core prover
  val provingActor = new ProvingActor with CoreResolutionProver1Factory



}



trait CoreResolutionProver1Factory {
  this: ProvingActor =>
  // configure the core prover

  val config = new Object {
    // the initial clause store

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)


    // unique literal resolver
    lazy val uniqueLiteralResolver = new DALCUniqueLiteralResolver(this)

    // ordered resolution needs comparator and selection
    lazy val precedence = new CustomConferencePartitionedPrecedence
    lazy val literalComparator = new ALCLPOComparator(this)
    lazy val selector = new DALCRSelector()

    // forward subsumer WITH index support
    lazy val forwardSubsumer = ForwardSubsumer

    lazy val backwardSubsumer = BackwardSubsumer

    // positive factorer
    lazy val positiveFactorer = new core.resolution.PositiveOrderedFactoring(this)

    // ACL resolver
    lazy val resolver = new DALCResolver(this)
    lazy val subsumptionStrategy = StillmannSubsumer
    lazy val inferenceRecorder = new NaiveClauseRecorder


    // usable clause store with STI indexes
    def usableClauseStore = new MutableClauseStore with LightestClauseHeuristicStorage with SForrestIndex
    def workedOffClauseStore = new MutableClauseStore with ListBufferStorage with SForrestIndex

    // switches
    // TODO enable all reductions

    val recordProofSteps = true

    // hard time limit
    val timeLimit: Long = 0;

  }


  val prover = new RobinsonProver(config)
  // setup this actor as a listener for resolution events
  prover.addObserver(this)


}




case class DALCReasoner extends ReasoningActor with DALCProverActorFactory with DALCDispatcherActorFactory with Neo4JLoggingActorFactory

case class DALCReasonerNoDispatch extends ReasoningActor with DALCProverActorFactory with ToVoidDispatcherActorFactory with Neo4JLoggingActorFactory

//case class DALCReasonerBroadCastDispatch extends ReasoningActor with DALCProverActorFactory with BroadCastDispatcherActorFactory










