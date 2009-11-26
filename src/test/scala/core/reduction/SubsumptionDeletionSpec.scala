package core.reduction

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 16:05:57
 */

import containers.{CNFClauseStore}
import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.jteigen.scalatest.JUnit4Runner

@RunWith(classOf[JUnit4Runner])
abstract class SubsumptionDeletionSpec extends Spec with ShouldMatchers with Logging {

  // override in specific tests
  val subsumptionDeleter: SubsumptionDeletion

  describe("A object implementing the SubsumptionDeletion Trait") {
    it("should delete subsumptions from clause store") {
      // init with the resolution example from the AIMA Book page 298

      val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val m2 = Constant("M2")
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


      val clauseStore = CNFClauseStore(StandardClause(Negation(Predicate("Missile", x))), StandardClause(Negation(Predicate("Missile", m1))), StandardClause(Predicate("Missile", m1), Predicate("Missile", m2)))

      subsumptionDeleter.deleteSubsumptions(clauseStore) should equal(CNFClauseStore(StandardClause(Negation(Predicate("Missile", x)))))


    }
    it("should delete subsumptions from clause store using other clause store as background kb") {
      // init with the resolution example from the AIMA Book page 298

      val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val m2 = Constant("M2")
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


      val inClauseStore = CNFClauseStore(StandardClause(Negation(Predicate("Missile", m1))), StandardClause(Predicate("Missile", m1), Predicate("Missile", m2)))
      val fromClauseStore = CNFClauseStore(StandardClause(Negation(Predicate("Missile", x))))
      // shoudl be completely reduzed
      subsumptionDeleter.deleteSubsumptions(inClauseStore, fromClauseStore) should equal(CNFClauseStore())


    }

  }
}