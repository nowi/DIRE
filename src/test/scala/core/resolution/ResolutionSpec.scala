package core.resolution

/**
 * User: nowi
 * Date: 04.11.2009
 * Time: 17:43:55
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import helpers.Logging
import reduction.{DuplicateLiteralDeleter, ClauseCondenser}

@RunWith(classOf[JUnit4Runner])
abstract class ResolutionSpec extends Spec with ShouldMatchers with Logging {
  implicit def setFolNode2StandardClause(set: Set[FOLNode]): StandardClause = StandardClause(set)

  val resolver: BinaryResolution
  val positiveFactorer: PositiveFactoring
  describe("A object implementing the Resolution Trait") {
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






    it("it should resolve the steps from the aima book page 298") {
      // init with the resolution example from the AIMA Book page 298

      // 1.) resolve goal clause with the C1
      val R1 = resolver(goalClause, C1)
      log.trace("R1 : %s", R1)
      //      R1 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z))))
      (List(Negation(american(west)), Negation(weapon(y)), Negation(sells(west, y, z)), Negation(hostile(z)))) should equal(R1.result)


      // 2.) resolve C7 with R1
      val R2 = resolver(C7, R1.result)
      log.trace("R2 : %s", R2)
      //      R2 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(hostile(z))))
      (List(Negation(weapon(y)), Negation(sells(west, y, z)),  Negation(hostile(z)))) should equal(R2.result)


      // 3.) resolve C4 with R2
      val R3 = resolver(C4, R2.result)
      log.trace("R3 : %s", R3)
      //      R3 should contain(Clause(Negation(sells(west, y, z)), Negation(missile(y)), Negation(hostile(z))))
      (List(Negation(missile(y)), Negation(sells(west, y, z)),  Negation(hostile(z)))) should equal(R3.result)


      // 4.) resolve C6 with R3
      val R4 = resolver(C6, R3.result)
      log.trace("R4 : %s", R4)
      //      R4 should contain(Clause(Negation(sells(west, m1, z)), Negation(hostile(z))))
      (List(Negation(sells(west, m1, z)), Negation(hostile(z)))) should equal(R4.result)


      // 5.) resolve C2 with R4
      val R5 = resolver(C2, R4.result)
      log.trace("R5 : %s", R5)
      (List(Negation(missile(m1)), Negation(owns(nono, m1)), Negation(hostile(nono)))) should equal(R5.result)


      // 6.) resolve C6 with R5
      val R6 = resolver(C6, R5.result)
      log.trace("R6 : %s", R6)
      (List(Negation(owns(nono, m1)), Negation(hostile(nono)))) should equal(R6.result)



      // 7.) resolve C5 with R6
      val R7 = resolver(C5, R6.result)
      log.trace("R7 : %s", R7)
      (List(Negation(hostile(nono)))) should equal(R7.result)



      // 8.) resolve C3 with R7
      val R8 = resolver(C3, R7.result)
      log.trace("R8 : %s", R8)
      (List(Negation(enemy(nono, america)))) should equal(R8.result)



      // 9.) resolve C8 with R8
      val R9 = resolver(C8, R8.result)
      log.trace("R9 : %s", R9)
      (Nil) should equal(R9.result)

    }



    it("should resolve the steps from the curiostiy kills the cat example") {
      // init with the resolution example from the AIMA Book page 298

      // 1.) resolve goal clause with the C1
      val R1 = resolver(E, F)
      log.debug("R1 : %s", R1)
      (List(animal(tuna))) should equal(R1.result)


      val R2 = resolver(R1.result, B)
      log.debug("R2 : %s", R2)
      (List(Negation(kills(x, tuna)),Negation(loves(z, x))) ) should equal(R2.result)

      val R2_1 = resolver(D, goalClauseCuriosity)
      log.debug("R2_1 : %s", R2_1)
      (List(kills(jack, tuna))) should equal(R2_1.result)


      val R3 = resolver(R2.result, R2_1.result)
      log.debug("R3 : %s", R3)
      (List(Negation(loves(z, jack)))) should equal(R3.result)

      val R3_1 = resolver(A2, C)
      log.debug("R3_1 : %s", R3_1)
      (List(Negation(animal(f(jack))), loves(g(jack), jack))) should equal(R3_1.result)


      val R4 = resolver(R3_1.result, A1)
      log.debug("R4 : %s", R4)
//      R4.result intersect (List(loves(g(jack), jack))) should equal(R4.result)
      val R4factored = positiveFactorer(R4.result)
      (List(loves(g(jack), jack))) should equal (R4factored)

      // attention , R4 needs factoring after resolution


      val R5 = resolver(R4factored, R3.result)
      log.info("R5 : %s", R5)
      (Nil) should equal(R5.result)


    }


    it("should resolve [¬(Hydrophobic(U))∨Hydrophobicity(U)+] + [¬(F(U))∨Hydrophobic(skf0197(U))+] --> Set([¬(F(skf0197(U_192)))∨Hydrophobicity(skf0197(U_192))])") {
      // [¬(Hydrophobic(U))∨Hydrophobicity(U)+] + [¬(F(U))∨Hydrophobic(skf0197(U))+]
      // --> Set([¬(F(skf0197(U_192)))∨Hydrophobicity(skf0197(U_192))])

      // should be Derived: 104[0:Res:28.1,4.0] || F(U) -> Hydrophobicity(skf0_197(U))*.

      val u = Variable("U")
      val clause1 = StandardClause(Negation(Predicate("Hydrophobic", u)), Predicate("Hydrophobicity", u))
      val clause2 = StandardClause(Negation(Predicate("F", u)), Predicate("Hydrophobic", Function("skf0197", u)))
      val resolvent = StandardClause(Negation(Predicate("F", u)), Predicate("Hydrophobicity", Function("skf0197", u)))

      (resolver(clause1, clause2).result match {
        case List(Negation(Predicate("F", List(Variable(_)))), Predicate("Hydrophobicity", List(Function("skf0197", List(Variable(_)))))) => true
        case _ => false

      }) should be(true)


    }

    it("should resolve cascaded substitution example") {
      val R3_1 = resolver(A2, C)
      log.debug("R3_1 : %s", R3_1)
      (List(Negation(animal(f(jack))), loves(g(jack), jack))) should equal(R3_1.result)
    }

    it("should resolve ") {
      val R2_1 = resolver(D, goalClauseCuriosity)
      log.debug("R2_1 : %s", R2_1)
      (List(kills(jack, tuna))) should equal(R2_1.result)
    }
    //




  }
}