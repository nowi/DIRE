package core.reduction

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 15:06:28
 */
import config.DALCConfig
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
  val subsumes: Subsumption

  // init with the resolution example from the AIMA Book page 298



  describe("A object implementing the Subsumption Trait") {
    val a = Constant("a")
    val b = Constant("b")
    val c = Constant("c")
    val d = Constant("d")
    val g = (node: FOLNode) => Function("g", node)
    val f = (node1: FOLNode, node2: FOLNode) => Function("f", node1, node2)
    val P = (node1: FOLNode, node2: FOLNode) => Predicate("P", node1, node2)
    val Q = (node1: FOLNode) => Predicate("Q", node1)
    val R = (node1: FOLNode) => Predicate("R", node1)

    val u = Variable("u")


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




    it("should test if c5 subsumes c6") {
      // test if c5 subsumes c6
      subsumes(C3, C4) should be(false)
    }

    it("Missile variable should subsume concrete missile ") {
      subsumes(StandardClause(Predicate("Missile", x)), StandardClause(Predicate("Missile", m1))) should be(true)
    }

    it("concrete missile should NOT subsume variable missile") {
      subsumes(StandardClause(Predicate("Missile", m1)), StandardClause(Predicate("Missile", x))) should be(false)
    }


    it("Missile variable should subsume concrete missile with negations") {
      subsumes(StandardClause(Negation(Predicate("Missile", x))), StandardClause(Negation(Predicate("Missile", m1)))) should be(true)
    }

    it("negated Missile variable should NOT subsume concrete missile") {
      subsumes(StandardClause(Negation(Predicate("Missile", x))), StandardClause(Predicate("Missile", m1))) should be(false)
    }


    it("subswumtion of redundant information") {
      subsumes(StandardClause(Predicate("Missile", x)), StandardClause(Predicate("Missile", m1), Predicate("Missile", m2))) should be(true)
    }


    it("should find sthe subsumption in teh example from page 248 term indexing - peter graf") {
      subsumes(StandardClause(P(x, b), Q(y)), StandardClause(P(a, b), Q(z), R(g(z)))) should be(true)
      subsumes(StandardClause(P(x, b), Q(x)), StandardClause(P(a, b), Q(z), R(g(z)))) should be(false) // because x cannot be gound to 2 terms simultanously

    }

    it("subsumer cannot be smaller then subsumed clause") {
      subsumes(StandardClause(P(a, x), P(y, b)), StandardClause(P(a, b))) should be(false)
    }

    it("should subsume a case from conference set"){
      val u = Variable("U")
      val subsumer = domain.fol.ast.ALCDClause(Predicate("P1",u),Negation(Predicate("P2",u)))
      val subsumed = domain.fol.ast.ALCDClause(Predicate("P1",u),Negation(Predicate("P2",u)),Negation(Predicate("P3",u)))

      subsumes(subsumer, subsumed) should be(true)
//      subsumes(subsumed, subsumer) should be(true)
    }

    it("should subsume a case from conference set -- different order of literals"){
      val u = Variable("U")
      val subsumer = domain.fol.ast.ALCDClause(Predicate("P1",u),Negation(Predicate("P2",u)))
      val subsumed = domain.fol.ast.ALCDClause(Negation(Predicate("P3",u)),Predicate("P1",u),Negation(Predicate("P2",u)))

      subsumes(subsumer, subsumed) should be(true)
//      subsumes(subsumed, subsumer) should be(true)
    }


    it("should subsume a same clauses"){
      val u = Variable("U")
      val subsumer = domain.fol.ast.ALCDClause(Negation(Predicate("P3",u)),Predicate("P1",u),Negation(Predicate("P2",u)))
      val subsumed = domain.fol.ast.ALCDClause(Negation(Predicate("P3",u)),Predicate("P1",u),Negation(Predicate("P2",u)))

      subsumes(subsumer, subsumed) should be(true)
//      subsumes(subsumed, subsumer) should be(true)
    }






  }
}