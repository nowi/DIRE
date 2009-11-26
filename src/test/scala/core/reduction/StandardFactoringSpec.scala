package core.reduction

/**
 * User: nowi
 * Date: 01.11.2009
 * Time: 14:31:26
 */

import com.jteigen.scalatest.JUnit4Runner

import config.TheoremProvingConfig1
import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class StandardFactoringSpec extends Spec with ShouldMatchers {
  describe("A object implementing the StandardFactoring Trait") {
    it("should factorize some clauses") {
      // init with the resolution example from the AIMA Book page 298

      val factorizer = new StandardFactorizer(TheoremProvingConfig1)


      val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val america = Constant("America")


      val C1 = StandardClause(Negation(Predicate("American", x)), Predicate("Weapon", y),
        Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
        Predicate("Criminal", x))

      val C2 = StandardClause(
        Negation(Predicate("Missile", x)),
        Negation(Predicate("Owns", nono, x)),
        Predicate("Sells", west, x, nono)
        )

      val C3 = StandardClause(
        Negation(Predicate("Enemy", x, america)),
        Predicate("Hostile", x)
        )


      val C4 = StandardClause(
        Negation(Predicate("Missile", x)),
        Predicate("Weapon", x)
        )

      val C5 = StandardClause(
        Predicate("Owns", nono, m1)
        )
      val C6 = StandardClause(
        Predicate("Missile", m1)
        )
      val C7 = StandardClause(
        Predicate("American", west)
        )

      val C8 = StandardClause(
        Predicate("Enemy", nono, america)
        )




      val clauseStore = CNFClauseStore(C1, C2, C3, C4, C5, C6, C7, C8)


      // create a proover

      // factorize a store
      factorizer.factorize(clauseStore)


    }

    it("should factorize Loves(G(Jack),Jack) OR Loves(G(x),x)") {
      // init with the resolution example from the AIMA Book page 298

      val factorizer = new StandardFactorizer(TheoremProvingConfig1)


      val jack = Constant("Jack")
      val x = Variable("x")
      val p1 = Predicate("Loves", Function("G", jack), jack)
      val p2 = Predicate("Loves", Function("G", x), x)

      val clause = StandardClause(p1, p2)
      val y = Variable("y")
      // factorize
      factorizer.factorize(clause) should equal(StandardClause(p1))


    }

    it("should factorize sentence with double variable predicate") {
      // init with the resolution example from the AIMA Book page 298

      val factorizer = new StandardFactorizer(TheoremProvingConfig1)

      val y = Variable("x")
      val x = Variable("y")

      val C = StandardClause(Predicate("man", x), Predicate("man", y), Negation(Predicate("in_love", x, y)))
      // x gets rewritten to y  , we need logical equals methods
      val C1 = StandardClause(Predicate("man", y), Negation(Predicate("in_love", y, y)))


      // factorize
      factorizer.factorize(C) should equal(C1)


    }


  }
}