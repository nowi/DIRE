package core.unification

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import rewriting.{VariableRewriter}

@RunWith(classOf[JUnit4Runner])
class RobinsonUnifierSpec extends Spec with ShouldMatchers with Logging {
  describe("The FOL Term Unifier ") {
    val john = Constant("John")
    val jane = Constant("Jane")
    val leonid = Constant("Leonid")
    val elizabeth = Constant("Elizabeth")
    val a = Function("Knows", List(Constant("John"), Variable("x")))
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


    // create unificator
    val unificator  = new  RobinsonUnificator

    it("should trivial unification") {
      // unificator.unify a and b
      val theta = unificator( Function("Knows", List(Constant("John"), Variable("x"))),  Function("Knows", List(Constant("John"), Variable("x"))))
      println(theta)
      theta should equal(Some(Map()))
    }

    it("should trivial unification -- shared term version") {
      // unificator.unify a and b
      val theta = unificator( Function("Knows", List(Constant("John"), Variable("x"))).shared,  Function("Knows", List(Constant("John"), Variable("x"))).shared)
      println(theta)
      theta should equal(Some(Map()))
    }

    it("should unificator.unify(Knows(John,x), Knows( John, Jane)) - {x/Jane}") {
      // unificator.unify a and b
      val theta = unificator(a, b)
      println(theta)
      theta should equal(Some(Map(Variable("x") -> jane)))
    }

    it("should unificator.unify(Knows(John,x), Knows( John, Jane)) - {x/Jane} -- shared terms version") {
      // unificator.unify a and b
      val theta = unificator(a.shared, b.shared)
      println(theta)
      theta should equal(Some(Map(Variable("x").shared -> jane.shared)))
    }

    it("should NOT unify -P(y) with P(a)") {
      val y = Variable("y")
      val a = Constant("a")

      val P1 = Negation(Predicate("P", y))
      // x gets rewritten to y  , we need logical equals methods
      val P2 = Predicate("P", a)

      // unificator.unify a and b
      val theta = unificator(P1, P2)
      println(theta)
      theta should be(None)

    }

    it("should NOT unify -P(y) with P(a) -- shared version") {
      val y = Variable("y").shared
      val a = Constant("a").shared

      val P1 = Negation(Predicate("P", y)).shared
      // x gets rewritten to y  , we need logical equals methods
      val P2 = Predicate("P", a).shared

      // unificator.unify a and b
      val theta = unificator(P1, P2)
      println(theta)
      theta should be(None)

    }

    it("should unificator.unify(Knows(John, x), Knows(y, Leonid)) = {x/Leonid, ylJohn} ") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart

      // unificator.unify a and c
      val theta2 = unificator(a, c)
      println(theta2)
      theta2 should equal(Some(Map(Variable("x") -> leonid, Variable("y") -> john)))

    }

    it("should unificator.unify(Knows(John, x), Knows(y, Leonid)) = {x/Leonid, ylJohn} -- shared version ") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart

      // unificator.unify a and c
      val theta2 = unificator(a.shared, c.shared)
      println(theta2)
      theta2 should equal(Some(Map(Variable("x").shared -> leonid.shared, Variable("y").shared -> john.shared)))

    }


    it("should unificator.unify(Knows(John,x), Know s(y,Mother (y))) = {ylJohn,xlMother(John)}") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart

      // unificator.unify a and d  -- this will test cascaded substituion ...
      val theta3 = unificator(a, d)
      println(theta3)
      theta3 should equal(Some(Map(Variable("y") -> john, Variable("x") -> Function("Mother", List(john)))))


    }


    it("should NOT unify(Knows(John,x), Know s(x, Elizabeth)) standardizing apart is needed") {
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
      unificator(a, e) should equal (None)
      // after normalization it should still not unify it
      unificator(a.normalize, e.normalize) should equal (None)

      // TODO should unify after standardize apart
    }

    it("should fail unifiying non linear term") {
      unificator(Function("f",x,x),Function("f",Constant("a"),Constant("b"))) should equal (None)
    }

    it("should fail unifiying non linear term -- shared versiono") {
      unificator(Function("f",x,x).shared,Function("f",Constant("a"),Constant("b")).shared) should equal (None)
    }

    it("should fail because of circular substitution , occurs check !") {
      unificator(Function("f",x,x),Function("f",Variable("y"),Function("g",Variable("y")))) should equal (None)
    }


    it("should fail because of circular substitution , occurs check ! - shared version") {
      unificator(Function("f",x,x).shared,Function("f",Variable("y"),Function("g",Variable("y"))).shared) should equal (None)
    }


    it("unify Tiny(U)*  Tiny(skf0_164(U))*  U --> skf0_164(U) .. should fail because of occurs check") {
      val u = Variable("U")
      //val u132 = Variable("U_1336")
      val a = Predicate("Tiny", Function("skf0164", u))
      val b = Predicate("Tiny", u)
      val mgu = unificator(a, b)
      log.warning("MGU is %s", mgu)
//      mgu should equal(Some(Map(u -> Function("skf0164", u))))
      mgu should equal(None)
    }



  }
}
