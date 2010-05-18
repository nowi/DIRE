package core


import com.jteigen.scalatest.JUnit4Runner
import net.lag.configgy.Configgy
import org.junit.runner.RunWith
import containers._
import domain.fol.ast.PositiveFOLLiteral
import heuristics.{LightestClauseHeuristicStorage, ListBufferStorage}
import ordering.{CustomConferencePartitionedPrecedence, ALCLPOComparator}
import recording.{Neo4JRecorder, NaiveClauseRecorder}
import reduction.{BackwardSubsumer, ForwardSubsumer, StillmannSubsumer}
import resolution._
import rewriting.VariableRewriter
import selection.{DALCRSelector, NegativeLiteralsSelection}

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 21:49:18
 */

class RobinsonProverBinaryResolutionWithForwardReductionNoIndexSpec extends ProvingSpec {
  // create the configuration here
  val config = new Object {
    // the initial clause store

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)

    val recordProofSteps = true

    lazy val neo4JGraphBasePath: String = "/workspace/DIRE/DIRE/logs/graph/clauses"
    lazy val inferenceRecorder = new Neo4JRecorder(neo4JGraphBasePath + "/" + System.currentTimeMillis + "/" + this)

    // positive factorer
    lazy val positiveFactorer = PositiveFactorer

    // binary resolver
    lazy val resolver = new BinaryResolver(this)

    // forward subsumer without index support
    lazy val forwardSubsumer = ForwardSubsumer

    // the backwardsubsumer
    lazy val backwardSubsumer = BackwardSubsumer

    lazy val subsumptionStrategy = StillmannSubsumer

    // usable clause store with STI indexes and lightesclauseheuristic
    def usableClauseStore = new MutableClauseStore() with LightestClauseHeuristicStorage

    def workedOffClauseStore = new MutableClauseStore() with ListBufferStorage


    // switches
    // TODO disable all reductions


    // hard time limit
    val timeLimit: Long = 5000;

  }

  override def createProver = new RobinsonProver(config)
}


class RobinsonProverBinaryResolutionWithOrderingWithForwardReductionNoIndexSpec extends ProvingSpec {
  // create the configuration here
  val config = new Object {
    // the initial clause store

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)

    val recordProofSteps = true

    lazy val neo4JGraphBasePath: String = "/workspace/DIRE/DIRE/logs/graph/clauses"
    lazy val inferenceRecorder = new Neo4JRecorder(neo4JGraphBasePath + "/" + System.currentTimeMillis + "/" + this)

    // positive factorer
    lazy val positiveFactorer = PositiveFactorer


    // selector
    lazy val selector = new NegativeLiteralsSelection


    // TODO refactor this
    // precedence precomputes the order , needs the initial clauses
    lazy val precedence = core.ordering.LazyLexicographicPrecedence

    // ordered resolution needs comparator and selection too
    lazy val literalComparator = new ALCLPOComparator(this)

    // literal ordering


    // binary resolver
    lazy val resolver = new OrderedBinaryResolver(this)

    // forward subsumer without index support
    lazy val forwardSubsumer = ForwardSubsumer

    // the backwardsubsumer
    lazy val backwardSubsumer = BackwardSubsumer

    lazy val subsumptionStrategy = StillmannSubsumer

    // usable clause store with STI indexes and lightesclauseheuristic
    def usableClauseStore = new MutableClauseStore() with LightestClauseHeuristicStorage

    def workedOffClauseStore = new MutableClauseStore() with ListBufferStorage


    // switches
    // TODO disable all reductions


    // hard time limit
    val timeLimit: Long = 10000;

  }

  override def createProver = new RobinsonProver(config)
}

class RobinsonProverBinaryResolutionWithForwardReductionWithIndexSpec extends ProvingSpec {
  // create the configuration here
  val config = new Object {
    // the initial clause store

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)

    val recordProofSteps = true

    lazy val neo4JGraphBasePath: String = "/workspace/DIRE/DIRE/logs/graph/clauses"
    lazy val inferenceRecorder = new Neo4JRecorder(neo4JGraphBasePath + "/" + System.currentTimeMillis + "/" + this)

    // positive factorer
    lazy val positiveFactorer = PositiveFactorer


    // selector
    lazy val selector = new NegativeLiteralsSelection


    // TODO refactor this
    // precedence precomputes the order , needs the initial clauses
    lazy val precedence = core.ordering.LazyLexicographicPrecedence

    // ordered resolution needs comparator and selection too
    lazy val literalComparator = new ALCLPOComparator(this)

    // literal ordering


    // binary resolver
    lazy val resolver = new BinaryResolver(this)

    // forward subsumer without index support
    lazy val forwardSubsumer = ForwardSubsumer

    // the backwardsubsumer
    lazy val backwardSubsumer = BackwardSubsumer

    lazy val subsumptionStrategy = StillmannSubsumer

    // usable clause store with STI indexes and lightesclauseheuristic
    def usableClauseStore = new MutableClauseStore() with LightestClauseHeuristicStorage with STIndex

    def workedOffClauseStore = new MutableClauseStore() with ListBufferStorage with STIndex with UnifiableClauseRetrieval


    // switches
    // TODO disable all reductions


    // hard time limit
    val timeLimit: Long = 10000;;

  }

  override def createProver = new RobinsonProver(config)
}


class RobinsonProverBinaryResolutionWithOrderingWithForwardReductionWithSTIIndexSpec extends ProvingSpec {
  // create the configuration here
  val config = new Object {
    // the initial clause store

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)

    val recordProofSteps = true

    lazy val neo4JGraphBasePath: String = "/workspace/DIRE/DIRE/logs/graph/clauses"
    lazy val inferenceRecorder = new Neo4JRecorder(neo4JGraphBasePath + "/" + System.currentTimeMillis + "/" + this)

    // positive factorer
    lazy val positiveFactorer = PositiveFactorer


    // selector
    lazy val selector = new NegativeLiteralsSelection


    // TODO refactor this
    // precedence precomputes the order , needs the initial clauses
    lazy val precedence = core.ordering.LazyLexicographicPrecedence

    // ordered resolution needs comparator and selection too
    lazy val literalComparator = new ALCLPOComparator(this)

    // literal ordering


    // binary resolver
    lazy val resolver = new OrderedBinaryResolver(this)

    // forward subsumer without index support
    lazy val forwardSubsumer = ForwardSubsumer

    // the backwardsubsumer
    lazy val backwardSubsumer = BackwardSubsumer

    lazy val subsumptionStrategy = StillmannSubsumer

    // usable clause store with STI indexes and lightesclauseheuristic
    def usableClauseStore = new MutableClauseStore() with LightestClauseHeuristicStorage with STIndex

    def workedOffClauseStore = new MutableClauseStore() with ListBufferStorage  with STIndex with UnifiableClauseRetrieval


    // switches
    // TODO disable all reductions


    // hard time limit
    val timeLimit: Long = 0;

  }

  override def createProver = new RobinsonProver(config)
}


class RobinsonProverBinaryResolutionNoReductionSTIIndexSpec extends ProvingSpec {
  // create the configuration here
  val config = new Object {
    // the initial clause store

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)
    val recordProofSteps = true


    // positive factorer
    lazy val positiveFactorer = PositiveFactorer
    // binary resolver
    lazy val resolver = new BinaryResolver(this)


    // forward subsumer without index support
    lazy val forwardSubsumer = ForwardSubsumer

    // the backwardsubsumer
    lazy val backwardSubsumer = BackwardSubsumer


    lazy val subsumptionStrategy = StillmannSubsumer
    lazy val inferenceRecorder = new NaiveClauseRecorder

    // usable clause store with STI indexes
    lazy val usableClauseStore = new MutableClauseStore() with LightestClauseHeuristicStorage with STIndex
    lazy val workedOffClauseStore = new MutableClauseStore() with ListBufferStorage with STIndex


    // switches
    // TODO disable all reductions


    // hard time limit
    val timeLimit: Long = 0;

  }

  override def createProver = new RobinsonProver(config)
}

class RobinsonProverBinaryResolutionWithReductionSTIIndexSpec extends ProvingSpec {
  // create the configuration here
  val config = new Object {
    // the initial clause store

    lazy val variableRewriter = new VariableRewriter
    lazy val standardizer = new Standardizer(this)

    // forward subsumer WITH index support
    lazy val forwardSubsumer = ForwardSubsumer

    // the backwardsubsumer
    lazy val backwardSubsumer = BackwardSubsumer

    // positive factorer
    lazy val positiveFactorer = PositiveFactorer
    // binary resolver
    lazy val resolver = new BinaryResolver(this)
    lazy val subsumptionStrategy = StillmannSubsumer
    lazy val inferenceRecorder = new NaiveClauseRecorder

    // usable clause store with default index and head index ( this is needed For forward matching in indexed forward subsumer
    lazy val usableClauseStore = new MutableClauseStore with LightestClauseHeuristicStorage with STIndex with STHeadIndex
    lazy val workedOffClauseStore = new MutableClauseStore with ListBufferStorage with STIndex with STHeadIndex

    // switches
    // TODO enable all reductions

    val recordProofSteps = true

    // hard time limit
    val timeLimit: Long = 0;

  }

  override def createProver = new RobinsonProver(config)

}


@RunWith(classOf[JUnit4Runner])
class RobinsonProverALCDResolutionWithReductionSTIIndexSpec extends ConferencePartitionedProvingSpec {
  // create the configuration here
  val config = new Object {
    // the initial clause store
    Configgy.configure("config/config.conf")

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

    // the backwardsubsumer
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

  override def createProver = new RobinsonProver(config)
}


@RunWith(classOf[JUnit4Runner])
class RobinsonProverMergedALCDResolutionWithReductionSTIIndexSpec extends ConferenceMergedProvingSpec {
  // create the configuration here
  val config = new Object {
    Configgy.configure("config/config.conf")



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

    // the backwardsubsumer
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

  override def createProver = new RobinsonProver(config)
}


@RunWith(classOf[JUnit4Runner])
class RobinsonProverMergedALCDResolutionWithReductionFeatureVectorImperfectIndexSpec extends ConferenceMergedProvingSpec {
  // create the configuration here
  val config = new Object {
    Configgy.configure("config/config.conf")



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

    // the backwardsubsumer
    lazy val backwardSubsumer = BackwardSubsumer


    // positive factorer
    lazy val positiveFactorer = new core.resolution.PositiveOrderedFactoring(this)

    // ACL resolver
    lazy val resolver = new DALCResolver(this)
    lazy val subsumptionStrategy = StillmannSubsumer
    lazy val inferenceRecorder = new NaiveClauseRecorder


    // usable clause store with STI indexes
    def usableClauseStore = new MutableClauseStore with LightestClauseHeuristicStorage with FeatureVectorImperfectIndex
    def workedOffClauseStore = new MutableClauseStore with ListBufferStorage with FeatureVectorImperfectIndex

    // switches
    // TODO enable all reductions

    val recordProofSteps = true

    // hard time limit
    val timeLimit: Long = 0;

  }

  override def createProver = new RobinsonProver(config)
}