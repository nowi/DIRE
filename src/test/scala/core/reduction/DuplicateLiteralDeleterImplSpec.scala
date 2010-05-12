package core.reduction

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 13:49:15
 */
import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import com.jteigen.scalatest.JUnit4Runner
import rewriting.{VariableRewriter}

@RunWith(classOf[JUnit4Runner])
class DuplicateLiteralDeleterImplSpec extends DuplicateLiteralDeletionSpec {
  override val deleter = DuplicateLiteralDeleter
}