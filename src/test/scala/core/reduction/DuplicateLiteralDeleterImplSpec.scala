package de.unima.dire.core.reduction

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 13:49:15
 */
import de.unima.dire.core.containers.{CNFClauseStore}
import de.unima.dire.core.rewriting.{VariableRewriter}

class DuplicateLiteralDeleterImplSpec extends DuplicateLiteralDeletionSpec {
  override val deleter = DuplicateLiteralDeleter
}