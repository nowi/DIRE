package core

/**
 * User: nowi
 * Date: 28.10.2009
 * Time: 16:37:19
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import core.{ProofFound}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


@RunWith(classOf[JUnit4Runner])
abstract class ProovingSpec extends Spec with ShouldMatchers {

  // create a proover
  val resolutionProover: Proving

  describe("A object implementing the Prooving Trait") {
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
    val sells = (x: FOLNode, y: FOLNode, z: FOLNode) => Predicate("Sells", x, y, z)
    val weapon = (x: FOLNode) => Predicate("Weapon", x)
    val american = (x: FOLNode) => Predicate("American", x)
    val hostile = (x: FOLNode) => Predicate("Hostile", x)
    val missile = (x: FOLNode) => Predicate("Missile", x)
    val owns = (x: FOLNode, y: FOLNode) => Predicate("Owns", x, y)
    val enemy = (x: FOLNode, y: FOLNode) => Predicate("Enemy", x, y)


    val C1 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
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

    val goalClause = StandardClause(
      Negation(Predicate("Criminal", west))
      )


    val goalClause2 = StandardClause(
      Negation(Predicate("American", west))
      )






    // the curiosity killed the cat domain
    val tuna = Constant("Tuna")
    val jack = Constant("Jack")
    val curiosity = Constant("Curiosity")
    val loves = (x: FOLNode, y: FOLNode) => Predicate("Loves", x, y)
    val kills = (x: FOLNode, y: FOLNode) => Predicate("Kills", x, y)
    val cat = (x: FOLNode) => Predicate("Cat", x)
    val animal = (x: FOLNode) => Predicate("Animal", x)
    val f = (x: FOLNode) => Predicate("F", x)
    val g = (x: FOLNode) => Predicate("G", x)


    val A1 = StandardClause(animal(f(x)), loves(g(x), x))
    val A2 = StandardClause(Negation(loves(x, f(x))), loves(g(x), x))
    val B = StandardClause(Negation(animal(y)), Negation(kills(x, y)), Negation(loves(z, x)))
    val C = StandardClause(Negation(animal(x)), loves(jack, x))
    val D = StandardClause(kills(jack, tuna), kills(curiosity, tuna))
    val E = StandardClause(cat(tuna))
    val F = StandardClause(Negation(cat(x)), animal(x))
    val goalClauseCuriosity = StandardClause(Negation(kills(curiosity, tuna)))















    it("should prove that west is a criminal") {



      // prove that west is a criminal
      resolutionProover.prove(CNFClauseStore(goalClause, C1, C2, C3, C4, C5, C6, C7, C8)) should equal(ProofFound()) // prove that west is a criminal




    }

    it("should prove that west is an american") {


      // prove that west is an american
      resolutionProover.prove(CNFClauseStore(goalClause2, C1, C2, C3, C4, C5, C6, C7, C8)) should equal(ProofFound())


    }


    it("should not refute that west is not american") {
      // create a proover
      resolutionProover.prove(CNFClauseStore(
        StandardClause(Predicate("American", west)), C1, C2, C3, C4, C5, C6, C7, C8)) should not equal (ProofFound())


    }

    it("should not refute that west is not criminal") {
      // create a proover
      resolutionProover.prove(CNFClauseStore(
        StandardClause(Predicate("Criminal", west)), C1, C2, C3, C4, C5, C6, C7, C8)) should not equal (ProofFound())


    }


    it("should prove that curiosity killed the cat") {
      // create a proover
      resolutionProover.prove(CNFClauseStore(A1, A2, B, C, D, E, F, goalClauseCuriosity)) should equal(ProofFound())


    }


  }
}
