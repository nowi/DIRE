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


      val one = Clause(Set(Predicate("dog", List(Variable("x"))), Predicate("animal", List(Variable("x")))))
      val two = Clause(Set(Predicate("dog", List(Constant("fido")))))
      val three = Clause(Set(Predicate("animal", List(Variable("y"))), Predicate("die", List(Variable("y")))))

      val clauseStore = CNFClauseStore(List(one, two, three))


      // create a proover

      val resolutionProover1 = new ResolutionProover1

      // prove
      resolutionProover1.prove(clauseStore)


    }


  }
}
