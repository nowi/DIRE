package de.unima.dire.domain.fol

/**
 * User: nowi
 * Date: 21.01.2010
 * Time: 13:48:59
 */

import de.unima.dire.domain.fol.ast._
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class FOLClauseSpec extends Spec with ShouldMatchers {
  describe("FOLClause") {
    it("should correctly implement the equality trait")
              {
                val u = Variable("U")
                val a = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))
                val b = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))


                assert(a === b)


              }
    it("alcd clause should correctly implement the equality trait")
              {
                val u = Variable("U")
                val a = new scala.collection.mutable.ListBuffer[FOLNode]()
                a ++= List(Predicate("Tiny", Variable("u")), Negation(Predicate("Small", Variable("u"))))
                val b = new scala.collection.mutable.ListBuffer[FOLNode]()
                b ++= List(Predicate("Tiny", Variable("u")), Negation(Predicate("Small", Variable("u"))))

                val aC = ALCDClause(a)
                val bC = ALCDClause(b)

                (a == b) should be(true)
                (aC == bC) should be(true)


              }


    it("alcd clause should correctly implement the equality trait for all negative literals")
              {
                val u = Variable("U")
                val a = new scala.collection.mutable.ListBuffer[FOLNode]()
                a ++= List(Negation(Predicate("Tiny", Variable("u"))), Negation(Predicate("Small", Variable("u"))))
                val b = new scala.collection.mutable.ListBuffer[FOLNode]()
                b ++= List((Predicate("Tiny", Variable("u"))).negate, Negation(Predicate("Small", Variable("u"))))

                val aC = ALCDClause(a)
                val bC = ALCDClause(b)

                (a == b) should be(true)
                (aC == bC) should be(true)


              }

    it("should correctly intersect sets ")
              {
                val u = Variable("U")
                val a = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))
                val b = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))

                val interseect = Set(a, b) ** Set(b, a)
                assert(interseect === Set(a, b))


              }
  }

}
