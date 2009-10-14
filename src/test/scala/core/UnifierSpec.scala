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
    it("should unify(Knows(John,x), Knows( John, Jane)) - {x/Jane}") {
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

    }

    it("should unify(Knows(John, x), Knows(y, Leonid)) = {x/Leonid, ylJohn} ") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart

      // unify a and c
      val theta2 = unify(a, c)
      println(theta2)
      theta2 should equal(Some(Map(Variable("x") -> leonid, Variable("y") -> john)))

    }


    it("should unify(Knows(John,x), Know s(y,Mother (y))) = {ylJohn,xlMother(John)}") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart

      // unify a and d  -- this will test cascaded substituion ...
      val theta3 = unify(a, d)
      println(theta3)
      theta3 should equal(Some(Map(Variable("y") -> john, Variable("x") -> Function("Mother", List(john)))))


    }


    it("should unify(Knows(John,x), Know s(x, Elizabeth)) by appliying standardize apart") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart


      // unfiy a and e -- this will test the standardise apart case
      val theta4 = unify(a, e)
      theta4 should not equal (None)

    }


  }
}
