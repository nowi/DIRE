package core.config


import ordering.ALCLPOComparator
import reduction.{StillmannSubsumer, StandardFactorizer, SubsumptionDeleter, TautologyDeleter}
import resolution.{OrderedResolver, BinaryResolver}
import rewriting.{Substitutor, VariableRewriter}
import selection.StandardClauseLiteralSelection

/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 14:44:06
 */


object TheoremProvingConfig1 {
  lazy val tautologyDeleter = new TautologyDeleter()
  lazy val variableRewriter = new VariableRewriter()
  lazy val subsumptionDeleter = new SubsumptionDeleter(this)
  lazy val standardizer = new Standardizer(this)
  lazy val unificator = new Unificator(this)
  lazy val substitutor = new Substitutor(this)
  lazy val factorizer = new StandardFactorizer(this)
  lazy val resolver = new BinaryResolver(this)
  lazy val subsumptionStrategy = new StillmannSubsumer(this)


}

object OrderedTheoremProvingConfig1 {
  lazy val tautologyDeleter = new TautologyDeleter()
  lazy val variableRewriter = new VariableRewriter()
  lazy val subsumptionDeleter = new SubsumptionDeleter(this)
  lazy val standardizer = new Standardizer(this)
  lazy val unificator = new Unificator(this)
  lazy val substitutor = new Substitutor(this)
  lazy val factorizer = new StandardFactorizer(this)
  lazy val resolver = new OrderedResolver(this)
  lazy val subsumptionStrategy = new StillmannSubsumer(this)
  // ordered resolution needs comparator and selection too
  lazy val comparator = new ALCLPOComparator()
  lazy val selector = new StandardClauseLiteralSelection()


}