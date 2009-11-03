package core

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import com.jteigen.scalatest.JUnit4Runner

import config.TheoremProvingConfig1
import domain.fol.ast._
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class StandardizerSpec extends Spec with ShouldMatchers {
  val standardizer = new Standardizer(TheoremProvingConfig1)

  val log = net.lag.logging.Logger.get
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
      val (x, y) = standardizer.standardizeApart(a, e)
      (x, y) should not equal ((a, e))

    }

    it("should unify Clause  A = {¬P (z , a), ¬P (z , x), ¬P (x, z )} union B = {P (z , f (z )), P (z , a)}.  ") {
      val x = Variable("x")
      val a = Variable("a")
      val z = Variable("z")
      // A = {¬P (z , a), ¬P (z , x), ¬P (x, z )}
      val A = Clause(Negation(Predicate("P", z, a)), Negation(Predicate("P", z, x)), Negation(Predicate("P", x, z)))
      val B = Clause(Predicate("P", z, Function("f", z)), Predicate("P", z, a))


      val theta5 = standardizer.standardizeApart(A, B)

      log.info("Standardized Apart tuple of clause A and B : %s is %s", A, theta5)
      theta5 should not equal ((A, B))

      // and there should be no vars in common



    }


  }


}
