package kernel

import dispatching.{ToVoidDispatchingActor, DALCDispatcherActor}
import domain.fol.ast.{FOLNode, FOLClause}
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import recording.{EventRecorder, NaiveClauseRecorder}
import se.scalablesolutions.akka.dispatch.Dispatchers
import core._
import caches.{ActorCache, SelectedLitCache, URLitCache, MaxLitCache}
import config.DALCConfig
import containers._
import containers.heuristics.{LightestClauseHeuristicStorage, ListBufferStorage}
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Subject
import java.io.File
import core.reduction._
import core.rewriting.{VariableRewriter}
import ordering.{LiteralComparison, CustomConferencePartitionedPrecedence, CustomSPASSModule1Precedence, ALCLPOComparator}
import ProvingState._
import ProvingResult._
import resolution._
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.util.Logging
import selection.{DALCRSelector, NegativeLiteralsSelection}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 02.03.2010
 * Time: 12:34:38
 */


class DefaultDALCReasoner extends ReasoningActor {
  println("%s is starting up.." format this)

  val config = new Object {
    // the initial clause store
    lazy val neo4JGraphBasePath: String = "logs/graph/clauses"
    val isDistributed = true
    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)


    // unique literal resolver
    lazy val uniqueLiteralResolver = Some(new DALCUniqueLiteralResolver(this))

    // ordered resolution needs comparator and selection
    lazy val precedence = new CustomConferencePartitionedPrecedence
    lazy val literalComparator = new ALCLPOComparator(this)
    lazy val selector = new DALCRSelector()

    // forward subsumer WITH index support
    lazy val forwardSubsumer = new ForwardSubsumer(this)

    lazy val backwardSubsumer = new BackwardSubsumer(this)

    // positive factorer
    lazy val positiveFactorer = new core.resolution.ALCPositiveOrderedFactoring(this)
    // negative factorer
    lazy val negativeFactorer = new ALCNegativeOrderedFactoring(this)

    // ACL resolver
    lazy val resolver = new DALCResolver(this)
    lazy val subsumptionStrategy = StillmannSubsumer
//    lazy val inferenceRecorder = Some(new NaiveClauseRecorder)
    lazy val inferenceRecorder = None
    lazy val eventRecorder = Some(new EventRecorder)
//    lazy val inferenceRecorder = None


    // the caches
    // chache for maximal literalas
    lazy val maxLitCache = new MaxLitCache()
    lazy val uniqueRLitCache = new URLitCache()
    lazy val selectedLitCache = new SelectedLitCache()
    lazy val actorCache = Some(new ActorCache())

    // usable clause store with STI indexes
    lazy val usableClauseStore = new MutableClauseStore with LightestClauseHeuristicStorage with FeatureVectorImperfectIndex
    lazy val workedOffClauseStore = new MutableClauseStore with ListBufferStorage with FeatureVectorImperfectIndex

    // switches
    // TODO enable all reductions

    val recordProofSteps = true

    // hard time limit
    val timeLimit: Long = 0;


    val prover = new RobinsonProver(this)

  }

  // setup this actor as a listener for resolution events

  // configure the core prover
  val provingActor = new ProvingActor(config)
  

  val dispatcherActor  = new DALCDispatcherActor(config)
//  val dispatcherActor  = new ToVoidDispatchingActors


  val derivationsLoggingActor = new Neo4JLoggingActor(config)

  val reductionsLoggingActor = new Neo4JLoggingActor(config)


}

class DefaultFOLReasoner extends ReasoningActor {
  println("%s is starting up.." format this)
// create the configuration here
  val config = new Object {
    // the initial clause store

    // THIS FLAG IS IMPORTANT
    val isDistributed = false


    // unique literal resolver
    lazy val uniqueLiteralResolver = None

    // ordered resolution needs comparator and selection
    lazy val precedence = core.ordering.LazyLexicographicPrecedence
    lazy val literalComparator = new Object with LiteralComparison {
      def compare(a: FOLNode, b: FOLNode) = Some(precedence.compare(a.top,b.top))
    }
    lazy val selector = new DALCRSelector()


     // forward subsumer WITH index support
    lazy val forwardSubsumer = new ForwardSubsumer(this)

    lazy val backwardSubsumer = new BackwardSubsumer(this)



    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)

    val recordProofSteps = true

    lazy val neo4JGraphBasePath: String = "logs/graph/clauses"
    //lazy val inferenceRecorder = Some(new Neo4JRecorder(neo4JGraphBasePath + "/" + System.currentTimeMillis + "/" + this))
    lazy val inferenceRecorder = None
    lazy val eventRecorder = Some(new EventRecorder)

    // positive factorer
    lazy val positiveFactorer = PositiveFactorer
     // negative factorer
    lazy val negativeFactorer = NegativeFactorer

    // binary resolver
    lazy val resolver = new BinaryResolver(this)

    // forward subsumer without index support

    lazy val subsumptionStrategy = StillmannSubsumer

    // usable clause store with STI indexes and lightesclauseheuristic
    lazy val usableClauseStore = new MutableClauseStore() with LightestClauseHeuristicStorage

    lazy val workedOffClauseStore = new MutableClauseStore() with ListBufferStorage

    // the caches
    // chache for maximal literalas
    lazy val maxLitCache = new MaxLitCache()
    lazy val uniqueRLitCache = new URLitCache()
    lazy val selectedLitCache = new SelectedLitCache()
    lazy val actorCache = Some(new ActorCache())


    // hard time limit
    val timeLimit: Long = 0;
    

    val prover = new RobinsonProver(this)


  }

  // setup this actor as a listener for resolution events

  // configure the core prover
  val provingActor = new ProvingActor(config)


  val dispatcherActor  = new DALCDispatcherActor(config)


  val derivationsLoggingActor = new Neo4JLoggingActor(config)

  val reductionsLoggingActor = new Neo4JLoggingActor(config)


}


//case class DALCReasonerBroadCastDispatch extends ReasoningActor with DALCProverActorFactory with BroadCastDispatcherActorFactory










