package de.unima.dire.core.config


import de.unima.dire.core.caches.{SelectedLitCache, URLitCache, MaxLitCache}
import de.unima.dire.core.containers.{CNFClauseStore}
import de.unima.dire.domain.fol.parsers.SPASSIntermediateFormatParser
import de.unima.dire.core.ordering.{CustomConferencePartitionedPrecedence, CustomSPASSModule1Precedence, ALCLPOComparator}
import de.unima.dire.recording.NaiveClauseRecorder
import de.unima.dire.core.reduction._
import de.unima.dire.core.resolution.{DALCResolver, DALCUniqueLiteralResolver}
import de.unima.dire.core.rewriting.{ VariableRewriter}
import de.unima.dire.core.selection.{NegativeLiteralsSelection}
import de.unima.dire.core.Standardizer

import java.io.File
/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 14:44:06
 */


object Partition1OrderedTheoremProvingConfig {
  // the initial clause store

  val parser = new SPASSIntermediateFormatParser()

  lazy val initialClauses = {
    // the curiosity killed the cat domain
    val file = new File("input/partitioned1clauses.spass")
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines // parse
    val clauses = parser.parseClauseStore(text)

    clauses match {
      case None => throw new IllegalStateException("Could not load clauses from file")
      case Some(clauseStore) => {
        clauseStore
      }
    }


  }

  lazy val variableRewriter = new VariableRewriter()
  lazy val standardizer = new Standardizer(this)
  lazy val resolver = new DALCResolver(this)
  lazy val subsumptionStrategy = StillmannSubsumer
  lazy val inferenceRecorder = Some(new NaiveClauseRecorder)
    lazy val uniqueLiteralResolver = Some(new DALCUniqueLiteralResolver(this))

  // ordered resolution needs comparator and selection too
  lazy val precedence = new CustomConferencePartitionedPrecedence
  lazy val literalComparator = new ALCLPOComparator(this)
  lazy val selector = new NegativeLiteralsSelection()

   // chache for maximal literalas
    lazy val maxLitCache = new MaxLitCache()
    lazy val uniqueRLitCache = new URLitCache()
    lazy val selectedLitCache = new SelectedLitCache()




  // settings
  val recordProofSteps = true
  val removeDuplicates = false
  val useLightesClauseHeuristic = true
  val usableBackSubsumption = false
  val forwardSubsumption = true
  val dropSeenClauses = false
  val useIndexing = true

  val timeLimit : Long = 0;

  override def toString = List(variableRewriter, standardizer, resolver, subsumptionStrategy, literalComparator, selector, removeDuplicates, useLightesClauseHeuristic)
          .map({_.toString})
          .reduceLeft(_ + ",\n" + _) + ("Precendece : \n %s" format (precedence))
}


