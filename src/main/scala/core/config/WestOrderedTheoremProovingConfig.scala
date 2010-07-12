package core.config


import caches.{MaxLitCache, URLitCache, SelectedLitCache}
import containers.{CNFClauseStore}
import ordering.{LazyLexicographicPrecedence, ALCLPOComparator}
import recording.NaiveClauseRecorder
import reduction._
import resolution.{DALCResolver, DALCUniqueLiteralResolver}
import rewriting.{ VariableRewriter}
import selection.{NegativeLiteralsSelection, DALCRSelector}
object WestOrderedTheoremProovingConfig {
  // the initial clause store
  lazy val initialClauses = {
    List(
      WestDomain.C1, WestDomain.C2, WestDomain.C3,
      WestDomain.C4, WestDomain.C5,
      WestDomain.C6, WestDomain.C7, WestDomain.C8)
  }

  lazy val variableRewriter = new VariableRewriter()
  lazy val standardizer = new Standardizer(this)
  lazy val resolver = new DALCResolver(this)
  lazy val subsumptionStrategy = StillmannSubsumer
  lazy val inferenceRecorder = Some(new NaiveClauseRecorder)

  // ordered resolution needs comparator and selection too
  lazy val precedence = core.ordering.LazyLexicographicPrecedence
  lazy val literalComparator = new ALCLPOComparator(this)
  lazy val selector = new NegativeLiteralsSelection()
  lazy val uniqueLiteralResolver = Some(new DALCUniqueLiteralResolver(this))

  
   // chache for maximal literalas
    lazy val maxLitCache = new MaxLitCache()
    lazy val uniqueRLitCache = new URLitCache()
    lazy val selectedLitCache = new SelectedLitCache()

  // settings
  val recordProofSteps = true
  val removeDuplicates = false
  val useLightesClauseHeuristic = true
  val usableBackSubsumption = true
  val forwardSubsumption = true
  val dropSeenClauses = false
  val useIndexing = true

  val timeLimit: Long = 0


  override def toString = List(variableRewriter, standardizer,  resolver, subsumptionStrategy, literalComparator, selector, removeDuplicates, useLightesClauseHeuristic)
          .map({_.toString})
          .reduceLeft(_ + ",\n" + _)
}