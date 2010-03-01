package core.config


import containers.{CNFClauseStore}
import ordering.{LexicographicPrecedence, ALCLPOComparator}
import reduction._
import resolution.{OrderedResolver}
import rewriting.{Substitutor, VariableRewriter}
import selection.{NegativeLiteralsSelection, ALCRSelector}
object WestOrderedTheoremProovingConfig {
  // the initial clause store
  lazy val initialClauses = {
    CNFClauseStore(
      WestDomain.C1, WestDomain.C2, WestDomain.C3,
      WestDomain.C4, WestDomain.C5,
      WestDomain.C6, WestDomain.C7, WestDomain.C8)
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
  lazy val precedence = new LexicographicPrecedence(this)
  lazy val literalComparator = new ALCLPOComparator(this)
  lazy val selector = new NegativeLiteralsSelection()

  // settings
  val recordProofSteps = true
  val removeDuplicates = false
  val useLightesClauseHeuristic = true
  val usableBackSubsumption = true
  val forwardSubsumption = true
  val dropSeenClauses = false
  val useIndexing = true

  val timeLimit: Long = 0


  override def toString = List(tautologyDeleter, variableRewriter, subsumptionDeleter, standardizer, unificator, substitutor, factorizer, resolver, subsumptionStrategy, literalComparator, selector, removeDuplicates, useLightesClauseHeuristic)
          .map({_.toString})
          .reduceLeft(_ + ",\n" + _)
}