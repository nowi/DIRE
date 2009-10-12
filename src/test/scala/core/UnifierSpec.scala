package core

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import org.junit.runner.RunWith
import Unifier._


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class UnifierSpec extends Spec with ShouldMatchers {
  describe("The FOL Term Unifier ") {
    it("should unify the example terms from the aima book") {

      //      val a = FOLDsl.parse2("Knows(John,x)");
      //      val b = FOLDsl.parse2("Knows(John,Jane)");
      //      val c = FOLDsl.parse2("Knows(y,Leonid)");
      //      val d = FOLDsl.parse2("Knows(y,Mother(y))");
      //      val e = FOLDsl.parse2("Knows(x,Elizabeth)"); // standardise apart case

      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")

      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart


      // unify a and b
      val theta = unify(a, b)
      println(theta)
      theta should equal(Some(Map(Variable("x") -> jane)))

      // unify a and c
      val theta2 = unify(a, c)
      println(theta2)
      theta2 should equal(Some(Map(Variable("x") -> leonid, Variable("y") -> john)))

      // unify a and d  -- this will test cascaded substituion ...
      val theta3 = unify(a, d)
      println(theta3)
      theta3 should equal(Some(Map(Variable("y") -> john, Variable("x") -> Function("Mother", List(john)))))



      assert(true)

    }


  }
}
