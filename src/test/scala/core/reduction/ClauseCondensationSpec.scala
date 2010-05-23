package core.reduction

/**
 * User: nowi
 * Date: 01.11.2009
 * Time: 14:31:26
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import rewriting.{VariableRewriter}

@RunWith(classOf[JUnit4Runner])
abstract class ClauseCondensationSpec extends Spec with ShouldMatchers {


  // we need a subsumption checker in the current scope for condesation of clauses


  val condenser  : ClauseCondensation


  describe("A object implementing the StandardFactoring Trait") {

    it("should factorize Loves(G(Jack),Jack) OR Loves(G(x),x)") {
      // init with the resolution example from the AIMA Book page 298

      implicit val subsumptionCheck = StillmannSubsumer

      val jack = Constant("Jack")
      val x = Variable("x")
      val p1 = Predicate("Loves", Function("G", jack), jack)
      val p2 = Predicate("Loves", Function("G", x), x)

      val clause = Set[FOLNode](p1, p2)
      val y = Variable("y")
      // factorize
      ClauseCondenser(clause)(subsumptionCheck) should equal(Set[FOLNode](p1))


    }

    it("should factorize sentence with double variable predicate") {
      // init with the resolution example from the AIMA Book page 298

       implicit val subsumptionCheck = StillmannSubsumer
      val y = Variable("x")
      val x = Variable("y")

      val C = Set[FOLNode](Predicate("man", x), Predicate("man", y), Negation(Predicate("in_love", x, y)))
      // x gets rewritten to y  , we need logical equals methods
      val C1 = Set[FOLNode](Predicate("man", y), Negation(Predicate("in_love", y, y)))


      // factorize
      ClauseCondenser(C)(subsumptionCheck) should equal(C1)


    }


  }
}