package core

/**
 * User: nowi
 * Date: 28.10.2009
 * Time: 16:37:19
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class ProovingSpec extends Spec with ShouldMatchers {
  describe("A object implementing the Prooving Trait") {
    it("should refute the clauses (Knows(John,x), Know s(x, Elizabeth))") {
      //¬dog(x) ∨ animal(x)
      //dog(fido)
      //¬animal(y) ∨ die(y)
      //
      //Negate the goal
      //¬die(fido)


      // init with the resolution example from the AIMA Book page 298

      val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val america = Constant("America")


      val C1 = Clause(Negation(Predicate("American", x)), Predicate("Weapon", y),
        Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
        Predicate("Criminal", x))

      val C2 = Clause(
        Negation(Predicate("Missile", x)),
        Negation(Predicate("Owns", nono, x)),
        Predicate("Sells", west, x, nono)
        )

      val C3 = Clause(
        Negation(Predicate("Enemy", x, america)),
        Predicate("Hostile", x)
        )


      val C4 = Clause(
        Negation(Predicate("Missile", x)),
        Predicate("Weapon", x)
        )

      val C5 = Clause(
        Predicate("Owns", nono, m1)
        )
      val C6 = Clause(
        Predicate("Missile", m1)
        )
      val C7 = Clause(
        Predicate("American", west)
        )

      val C8 = Clause(
        Predicate("Enemy", nono, america)
        )




      val clauseStore = CNFClauseStore(C1, C2, C3, C4, C5, C6, C7, C8)


      // create a proover

      val resolutionProover1 = new ResolutionProover1

      // prove
      resolutionProover1.prove(clauseStore)


    }


  }
}
