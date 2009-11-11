package core.reduction

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 15:06:28
 */
import containers.{CNFClauseStore}
import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.jteigen.scalatest.JUnit4Runner

@RunWith(classOf[JUnit4Runner])
abstract class SubsumptionSpec extends Spec with ShouldMatchers with Logging {

  // override in specific tests
  val subsumer: Subsumption

  describe("A object implementing the Subsumption Trait") {
    it("should subsume") {
      // init with the resolution example from the AIMA Book page 298

      val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val m2 = Constant("M2")
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




      // test if c5 subsumes c6
      subsumer.subsumes(C3, C4) should be(false)


      // Missile variable should subsume concrete missile
      subsumer.subsumes(Clause(Predicate("Missile", x)), Clause(Predicate("Missile", m1))) should be(true)


      // Missile variable should subsume concrete missile with negations
      subsumer.subsumes(Clause(Negation(Predicate("Missile", x))), Clause(Negation(Predicate("Missile", m1)))) should be(true)



      // subsumtion of redundant information
      subsumer.subsumes(Clause(Predicate("Missile", m1)), Clause(Predicate("Missile", m1), Predicate("Missile", m2))) should be(true)


    }

  }
}