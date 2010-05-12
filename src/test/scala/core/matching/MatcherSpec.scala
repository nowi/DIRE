package core.matching

/**
 * User: nowi
 * Date: 15.04.2010
 * Time: 10:59:26
 */

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import rewriting.{VariableRewriter}

@RunWith(classOf[JUnit4Runner])
class MatcherSpec extends Spec with ShouldMatchers with Logging {
  describe("The FOL Term Matcher ") {
    val john = Constant("John")
    val jane = Constant("Jane")
    val leonid = Constant("Leonid")
    val elizabeth = Constant("Elizabeth")
    val a = Function("Knows", List(john, Variable("x")))
    val b = Function("Knows", List(john, jane))
    val c = Function("Knows", List(Variable("y"), leonid))
    val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
    val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart


    val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val america = Constant("America")
      val sells = (x: FOLNode, y: FOLNode, z: FOLNode) => Predicate("Sells", x, y, z)
      val weapon = (x: FOLNode) => Predicate("Weapon", x)
      val american = (x: FOLNode) => Predicate("American", x)
      val hostile = (x: FOLNode) => Predicate("Hostile", x)
      val missile = (x: FOLNode) => Predicate("Missile", x)
      val owns = (x: FOLNode, y: FOLNode) => Predicate("Owns", x, y)
      val enemy = (x: FOLNode, y: FOLNode) => Predicate("Enemy", x, y)


    // create matcher
    val matcher  = new  Matcher

    it("should find trivial matcher") {
      // unificator.unify a and b
      val m = matcher.matcher(a, a)
      println(m)
      m should equal(Some(Map()))
    }


    it("should find easy matchers") {
      val instance = b
      val generalization = a

      val m = matcher.matcher(generalization, instance)
      println(m)
      m should equal(Some(Map(x -> jane)))
    }

    it("should find nested matches") {
      val instance = Function("Knows", List(john, a))
      val generalization = a

      val m = matcher.matcher(generalization, instance)
      println(m)
      m should equal(Some(Map(x -> a)))
    }

    it("should not find matcher") {
      val instance = Function("f", Constant("a"),Constant("b"))
      val generalization = Function("f", IndicatorVariable(0),Function("g",Constant("b")))

      val m = matcher.matcher(generalization, instance)
      println(m)
      m should equal(None)
    }


    it("should not find matcher 2") {

      val f = (t1 : FOLNode,t2:FOLNode) => Function("f",t1,t2)
      val g = (t1 : FOLNode) => Function("g",t1)

      val instance = f(Constant("c"),g(Constant("d")) )
      val generalization = f(Variable(),Variable())

      val m = matcher.matcher(generalization, instance)
      println(m)
      (m match {
        case Some(_) => true
        case _ => false
      }) should be (true)
    }

  }

}
