package core

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

import rewriting.VariableRewriter
import helpers.Logging

@RunWith(classOf[JUnit4Runner])
class StandardizerSpec extends Spec with ShouldMatchers with Logging{
  val config = new Object {
    val variableRewriter = new VariableRewriter()
  }

  val standardizer = new Standardizer(config)


  describe("The Standardizer") {
    it("should stardize (Knows(John,x), Know s(x, Elizabeth))") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart


      // standardize a and e
      val (x, y,renamings) = standardizer.standardizeApart(a, e)
      (x, y) should not equal ((a, e))

    }

    


  }


}
