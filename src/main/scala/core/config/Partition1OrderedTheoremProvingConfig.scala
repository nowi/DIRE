package core.config


import containers.{CNFClauseStore}
import domain.fol.parsers.SPASSIntermediateFormatParser
import java.io.File
import ordering.{CustomSPASSModule1Precedence, ALCLPOComparator}
import reduction._
import resolution.{OrderedResolver}
import rewriting.{Substitutor, VariableRewriter}
import selection.{NegativeLiteralsSelection}

/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 14:44:06
 */


object Partition1OrderedTheoremProvingConfig {
  // the initial clause store
  lazy val initialClauses = {
    // the curiosity killed the cat domain
    val file = new File("input/partitioned1clauses.spass")
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines // parse
    val clauses = SPASSIntermediateFormatParser.parseClauseStore(text)

    clauses match {
      case None => throw new IllegalStateException("Could not load clauses from file")
      case Some(clauseStore) => {
        clauseStore
      }
    }


  }

  lazy val tautologyDeleter = new TautologyDeleter()
  lazy val variableRewriter = new VariableRewriter()
  lazy val subsumptionDeleter = new SubsumptionDeleter(this)
  lazy val standardizer = new Standardizer(this)
  lazy val unificator = new Unificator(this)
  lazy val substitutor = new Substitutor(this)
  lazy val factorizer = new OrderedFactorizer(this)
  lazy val resolver = new OrderedResolver(this)
  lazy val subsumptionStrategy = new StillmannSubsumer(this)

  // ordered resolution needs comparator and selection too
  lazy val precedence = new CustomSPASSModule1Precedence
  lazy val literalComparator = new ALCLPOComparator(this)
  lazy val selector = new NegativeLiteralsSelection()

  // settings
  val recordProofSteps = true
  val removeDuplicates = false
  val useLightesClauseHeuristic = true
  val usableBackSubsumption = false
  val forwardSubsumption = true
  val dropSeenClauses = false
  val useIndexing = true

  val timeLimit : Long = 0;

  override def toString = List(tautologyDeleter, variableRewriter, subsumptionDeleter, standardizer, unificator, substitutor, factorizer, resolver, subsumptionStrategy, literalComparator, selector, removeDuplicates, useLightesClauseHeuristic)
          .map({_.toString})
          .reduceLeft(_ + ",\n" + _) + ("Precendece : \n %s" format (precedence))
}


