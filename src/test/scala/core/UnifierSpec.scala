package core

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import com.jteigen.scalatest.JUnit4Runner

import config.TheoremProvingConfig1
import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import rewriting.Substitutor

@RunWith(classOf[JUnit4Runner])
class UnifierSpec extends Spec with ShouldMatchers with Logging {
  describe("The FOL Term Unifier ") {

    // create unificator
    val unificator = new Unificator(TheoremProvingConfig1)

    val substitutor = new Substitutor(TheoremProvingConfig1)
    //    Configgy.configureFromResource("config.conf")



    it("should unificator.unify(Knows(John,x), Knows( John, Jane)) - {x/Jane}") {
      val john = Constant("John")
      val jane = Constant("Jane")
      val leonid = Constant("Leonid")
      val elizabeth = Constant("Elizabeth")
      val a = Function("Knows", List(john, Variable("x")))
      val b = Function("Knows", List(john, jane))
      val c = Function("Knows", List(Variable("y"), leonid))
      val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
      val e = Function("Knows", List(Variable("x"), elizabeth)) // standardise apart

      // unificator.unify a and b
      val theta = unificator.unify(a, b)
      println(theta)
      theta should equal(Some(Map(Variable("x") -> jane)))

    }

    it("should unify -P(y) with P(a)") {
      val y = Variable("y")
      val a = Constant("a")



      val P1 = Negation(Predicate("P", y))
      // x gets rewritten to y  , we need logical equals methods
      val P2 = Predicate("P", a)

      // unificator.unify a and b
      val theta = unificator.unify(P1, P2)
      println(theta)
      theta should equal(Some(Map(y -> a)))

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
      val theta2 = unificator.unify(a, c)
      println(theta2)
      theta2 should equal(Some(Map(Variable("x") -> leonid, Variable("y") -> john)))

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
      val theta3 = unificator.unify(a, d)
      println(theta3)
      theta3 should equal(Some(Map(Variable("y") -> john, Variable("x") -> Function("Mother", List(john)))))


    }


    it("should unificator.unify(Knows(John,x), Know s(x, Elizabeth)) by appliying standardize apart") {
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
      val theta4 = unificator.unify(a, e)
      theta4 should not equal (None)

    }


    it("should unify Clause  A = {¬P (z , a), ¬P (z , x), ¬P (x, z )} union B = {P (z , f (z )), P (z , a)}.  ") {
      val x = Variable("x")
      val a = Constant("a")
      val z = Variable("z")
      // A = {¬P (z , a), ¬P (z , x), ¬P (x, z )}
      val A = StandardClause(Negation(Predicate("P", z, a)), Negation(Predicate("P", z, x)), Negation(Predicate("P", x, z)))

      //      B = {P (z , f (z )), P (z , a)}.

      val B = StandardClause(Predicate("P", z, Function("f", z)), Predicate("P", z, a))

      //      A' = {¬P (z , a), ¬P (z , x)}
      val AStrich = StandardClause(Negation(Predicate("P", z, a)), Negation(Predicate("P", z, x)))
      // ′ = {P (z , a)}
      val BStrich = StandardClause(Predicate("P", z, a), Predicate("P", z, Function("f", z)))


      val anegstrich = A.negativeLiterals
      val bposstrich = B.positiveLiterals

      // unfiy a and e -- this will test the standardise apart case
      val theta5 = unificator.unify(StandardClause(anegstrich).absoluteClause, StandardClause(bposstrich))

      log.trace("MGU of union of a and b is {}", theta5)
      // the subsitutions should containt Some(Map(z_4 -> z_8, x -> a))
      theta5 should not equal (None)


    }

    it("should unify two Predicates double variable predicate") {
      // init with the resolution example from the AIMA Book page 298
      val y = Variable("x")
      val x = Variable("y")

      val C = StandardClause(Predicate("man", x), Predicate("man", y), Negation(Predicate("in_love", x, y)))
      //      val C1 = Clause(Predicate("man", x), Negation(Predicate("in_love", x, x)))

      // unfiy a and e -- this will test the standardise apart case
      val firstUnfier = unificator.firstUnifier(C)

      log.trace("First unifier of a and b is {}", firstUnfier)
      firstUnfier should not equal (None)

      // now rewrite the C clause with the first unifier
      val C1 = substitutor.substitute(firstUnfier, C)
      log.trace("Rewritten C to C1 using unifer {} == {}", firstUnfier, C1)
      C1 should not equal (C)

      // check if there are more substitions , should be none
      val secondUnifier = unificator.firstUnifier(C1)
      log.trace("Second unifier of a and b is {}", secondUnifier)
      secondUnifier should equal(None)
      // the subsitutions should containt Some(Map(z_4 -> z_8, x -> a))
    }

    it("should Variable with nested function level 1") {
      val u = Variable("U")
      val u132 = Variable("U_1336")
      val a = Predicate("Tiny", Function("skf0164", u132))
      val b = Negation(Predicate("Tiny", u))
      val mgu = unificator.unify(a, b)
      log.warn("MGU is {}", mgu)
      mgu should not equal (None)
    }

    it("should Variable with nested predicate level 1") {
      val u = Variable("U")
      val u132 = Variable("U_1336")
      val a = Predicate("Tiny", Predicate("skf0164", u132))
      val b = Negation(Predicate("Tiny", u))
      val mgu = unificator.unify(a, b)
      log.warn("MGU is {}", mgu)
      mgu should not equal (None)
    }








    //    it("should unify Clause  {P (z1 , a), P (z1 , x), P (z2 , a)} ") {
    //      val z1 = Variable("z1");
    //      val z2 = Variable("z2");
    //      val a = Variable("a")
    //      val x = Variable("x")
    //
    //      val p1 = Clause(Predicate("P", z1, a), Predicate("P", z1, x), Predicate("P", z2, a))
    //
    //
    //      // unfiy a and e -- this will test the standardise apart case
    //      val theta5 = unificator.unify(p1)
    //
    //
    //      log.trace("MGU of Clause : {} is {}", p1, theta5)
    //      theta5 should not equal (None)
    //
    //
    //    }


  }
}
