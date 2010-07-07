package de.unima.dire.core.reduction

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 15:38:02
 */

import de.unima.dire.core.containers.{CNFClauseStore}
import de.unima.dire.domain.fol.ast._
import de.unima.dire.core.rewriting.{VariableRewriter}

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class ClauseCondenserImplSpec extends ClauseCondensationSpec {
  override val condenser = ClauseCondenser
  
}