package core.resolution

/**
 * User: nowi
 * Date: 04.11.2009
 * Time: 17:43:55
 */

import com.jteigen.scalatest.JUnit4Runner

import config.TheoremProvingConfig1
import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class ResolutionSpec extends Spec with ShouldMatchers {
  val log = net.lag.logging.Logger.get
  val resolver = new BinaryResolver(TheoremProvingConfig1)
  describe("A object implementing the Resolution Trait") {
    //    it("should factorize some clauses") {
    //      // init with the resolution example from the AIMA Book page 298
    //
    //
    //
    //      val x = Variable("x")
    //      val y = Variable("y")
    //      val z = Variable("z")
    //      val west = Constant("West")
    //      val nono = Constant("Nono")
    //      val m1 = Constant("M1")
    //      val america = Constant("America")
    //
    //
    //      val C1 = Clause(Negation(Predicate("American", x)), Predicate("Weapon", y),
    //        Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
    //        Predicate("Criminal", x))
    //
    //      val C2 = Clause(
    //        Negation(Predicate("Missile", x)),
    //        Negation(Predicate("Owns", nono, x)),
    //        Predicate("Sells", west, x, nono)
    //        )
    //
    //      val C3 = Clause(
    //        Negation(Predicate("Enemy", x, america)),
    //        Predicate("Hostile", x)
    //        )
    //
    //
    //      val C4 = Clause(
    //        Negation(Predicate("Missile", x)),
    //        Predicate("Weapon", x)
    //        )
    //
    //      val C5 = Clause(
    //        Predicate("Owns", nono, m1)
    //        )
    //      val C6 = Clause(
    //        Predicate("Missile", m1)
    //        )
    //      val C7 = Clause(
    //        Predicate("American", west)
    //        )
    //
    //      val C8 = Clause(
    //        Predicate("Enemy", nono, america)
    //        )
    //
    //
    //
    //
    //      val clauseStore = CNFClauseStore(C1, C2, C3, C4, C5, C6, C7, C8)
    //
    //
    //      // create a proover
    //
    //      // factorize a store
    //      factorizer.factorize(clauseStore)
    //
    //
    //    }

    it("should resolve A = {¬P (z , a), ¬P (z , x), ¬P (x, z )} and B = {P (z , f (z )), P (z , a)}.") {
      // init with the resolution example from the AIMA Book page 298
      val x = Variable("x")
      val a = Constant("a")
      val z = Variable("z")
      // A = {¬P (z , a), ¬P (z , x), ¬P (x, z )}
      val A = Clause(Negation(Predicate("P", z, a)))
      //      B = {P (z , f (z )), P (z , a)}.
      val B = Clause(Predicate("P", z, Function("f", z)), Predicate("P", z, a))
      // resolve
      val conclusions1 = resolver.resolve(A, B) should not equal (None)
      log.info("Resolved : %s", conclusions1)

      // resolve different order
      val conclusions2 = resolver.resolve(B, A) should not equal (None)
      log.info("Resolved : %s", conclusions2)

    }


  }
}