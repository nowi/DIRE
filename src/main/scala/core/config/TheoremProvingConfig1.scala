package core.config


import reduction.{StandardFactorizer, SubsumptionDeleter, TautologyDeleter}
import resolution.BinaryResolver
import rewriting.{Substitutor, VariableRewriter}

/**
 * User: nowi
 * Date: 02.11.2009
 * Time: 14:44:06
 */


object TheoremProvingConfig1 {
  lazy val tautologyDeleter = new TautologyDeleter()
  lazy val variableRewriter = new VariableRewriter()
  lazy val subsumptionDeleter = new SubsumptionDeleter()
  lazy val standardizer = new Standardizer(this)
  lazy val unificator = new Unificator(this)
  lazy val substitutor = new Substitutor(this)
  lazy val factorizer = new StandardFactorizer(this)
  lazy val resolver = new BinaryResolver(this)


}