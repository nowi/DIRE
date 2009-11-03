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
class UnifierSpec extends Spec with ShouldMatchers {
  describe("The FOL Term Unifier ") {

    // create unificator
    val unificator = new Unificator(TheoremProvingConfig1)

    //    Configgy.configureFromResource("config.conf")

    // logger
    val log = net.lag.logging.Logger.get

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
      val a = Variable("a")
      val z = Variable("z")
      // A = {¬P (z , a), ¬P (z , x), ¬P (x, z )}
      val A = Clause(Negation(Predicate("P", z, a)), Negation(Predicate("P", z, x)), Negation(Predicate("P", x, z)))

      //      B = {P (z , f (z )), P (z , a)}.

      val B = Clause(Predicate("P", z, Function("f", z)), Predicate("P", z, a))

      val aneg = A.negativeLiterals
      val bpos = B.positiveLiterals

      // unfiy a and e -- this will test the standardise apart case
      val theta5 = unificator.unify(Clause(aneg ++ bpos))

      log.info("MGU of union of a and b is %s", theta5)
      theta5 should not equal (None)


    }





    it("should unify Clause  {P (z1 , a), P (z1 , x), P (z2 , a)} ") {
      val z1 = Variable("z1");
      val z2 = Variable("z2");
      val a = Variable("a")
      val x = Variable("x")

      val p1 = Clause(Predicate("P", z1, a), Predicate("P", z1, x), Predicate("P", z2, a))


      // unfiy a and e -- this will test the standardise apart case
      val theta5 = unificator.unify(p1)


      log.info("MGU of Clause : %s is %s", p1, theta5)
      theta5 should not equal (None)


    }


  }
}
