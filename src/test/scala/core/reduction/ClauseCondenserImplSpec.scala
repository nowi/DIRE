package core.reduction

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 15:38:02
 */
      import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import rewriting.{VariableRewriter}

@RunWith(classOf[JUnit4Runner])
class ClauseCondenserImplSpec extends ClauseCondensationSpec {
  override val condenser = ClauseCondenser
  
}