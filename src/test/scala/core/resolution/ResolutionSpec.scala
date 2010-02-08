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



@RunWith(classOf[JUnit4Runner])
abstract class ResolutionSpec extends Spec with ShouldMatchers with Logging {
  
  val resolver: Resolution
  describe("A object implementing the Resolution Trait") {
    it("it should resolve the steps from the aima book page 298") {
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

      // 1.) resolve goal clause with the C1
      val R1 = resolver.resolve(goalClause, C1)
      log.trace("R1 : %s", R1)
      R1 should have size (1)
      //      R1 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z))))
      R1.contains(StandardClause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z)))) should be(true)


      // 2.) resolve C7 with R1
      val R2 = resolver.resolve(C7, R1.toList.head)
      log.trace("R2 : %s", R2)
      R2 should have size (1)
      //      R2 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(hostile(z))))
      R2.contains(StandardClause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(hostile(z)))) should be(true)


      // 3.) resolve C4 with R2
      val R3 = resolver.resolve(C4, R2.toList.head)
      log.trace("R3 : %s", R3)
      R3 should have size (1)
      //      R3 should contain(Clause(Negation(sells(west, y, z)), Negation(missile(y)), Negation(hostile(z))))
      R3.contains(StandardClause(Negation(sells(west, y, z)), Negation(missile(y)), Negation(hostile(z)))) should be(true)


      // 4.) resolve C6 with R3
      val R4 = resolver.resolve(C6, R3.toList.head)
      log.trace("R4 : %s", R4)
      R4 should have size (1)
      //      R4 should contain(Clause(Negation(sells(west, m1, z)), Negation(hostile(z))))
      R4.contains(StandardClause(Negation(sells(west, m1, z)), Negation(hostile(z)))) should be(true)


      // 5.) resolve C2 with R4
      val R5 = resolver.resolve(C2, R4.toList.head)
      log.trace("R5 : %s", R5)
      R5 should have size (1)
      //      R5 should contain(Clause(Negation(missile(m1)), Negation(owns(nono, m1)), Negation(hostile(nono))))
      R5.contains(StandardClause(Negation(missile(m1)), Negation(owns(nono, m1)), Negation(hostile(nono)))) should be(true)


      // 6.) resolve C6 with R5
      val R6 = resolver.resolve(C6, R5.toList.head)
      log.trace("R6 : %s", R6)
      R6 should have size (1)
      //      R6 should contain(Clause(Negation(owns(nono, m1)), Negation(hostile(nono))))
      R6.contains(StandardClause(Negation(owns(nono, m1)), Negation(hostile(nono)))) should be(true)



      // 7.) resolve C5 with R6
      val R7 = resolver.resolve(C5, R6.toList.head)
      log.trace("R7 : %s", R7)
      R7 should have size (1)
      //      R7 should contain(Clause(Negation(hostile(nono))))
      R7.contains(StandardClause(Negation(hostile(nono)))) should be(true)



      // 8.) resolve C3 with R7
      val R8 = resolver.resolve(C3, R7.toList.head)
      log.trace("R8 : %s", R8)
      R8 should have size (1)
      assert(R8.contains(StandardClause(Negation(enemy(nono, america)))))
      //      R8  should contain(Clause(Negation(enemy(nono, america))))
      R8.contains(StandardClause(Negation(enemy(nono, america)))) should be(true)



      // 9.) resolve C8 with R8
      val R9 = resolver.resolve(C8, R8.toList.head)
      log.trace("R9 : %s", R9)
      //      R9 should contain(EmptyClause())
      R9.contains(EmptyClause()) should be(true)


    }

    it("some experiments") {
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


      val goalClause = StandardClause(
        Negation(Predicate("Criminal", west))
        )




      // 1.) resolve goal clause with the C1
      val R1 = resolver.resolve(C1, goalClause)
      log.trace("R1 : %s", R1)
      R1 should have size (1)
      //      R1 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z))))
      R1.contains(StandardClause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z)))) should be(true)



      // 1.) resolve goal clause with the C1
      val R2 = resolver.resolve(goalClause, C1)
      log.trace("R1 : %s", R2)
      R2 should have size (1)
      //      R1 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z))))
      R2.contains(StandardClause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z)))) should be(true)


    }



    it("should resolve the steps from the curiostiy kills the cat example") {
      // init with the resolution example from the AIMA Book page 298


      val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
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




      // 1.) resolve goal clause with the C1
      val R1 = resolver.resolve(E, F)
      log.debug("R1 : %s", R1)
      R1 should have size (1)
      R1.contains(StandardClause(animal(tuna))) should be(true)


      val R2 = resolver.resolve(R1.toList.head, B)
      log.debug("R2 : %s", R2)
      R2 should have size (1)
      R2.contains(StandardClause(Negation(loves(z, x)), Negation(kills(x, tuna)))) should be(true)

      val R2_1 = resolver.resolve(D, goalClauseCuriosity)
      log.debug("R2_1 : %s", R2_1)
      R2_1 should have size (1)
      R2_1.contains(StandardClause(kills(jack, tuna))) should be(true)


      val R3 = resolver.resolve(R2.toList.head, R2_1.toList.head)
      log.debug("R3 : %s", R3)
      R3 should have size (1)
      R3.contains(StandardClause(Negation(loves(z, jack)))) should be(true)

      val R3_1 = resolver.resolve(A2, C)
      log.debug("R3_1 : %s", R3_1)
      R3_1 should have size (1)
      R3_1.contains(StandardClause(Negation(animal(f(jack))), loves(g(jack), jack))) should be(true)


      val R4 = resolver.resolve(R3_1.toList.head, A1)
      log.debug("R4 : %s", R4)
      R4 should have size (1)
      R4.contains(StandardClause(loves(g(jack), jack))) should be(true)



      val R5 = resolver.resolve(R4.toList.head, R3.toList.head)
      log.debug("R5 : %s", R5)
      R5 should have size (1)
      R5.contains(EmptyClause()) should be(true)


    }


    it("should resolve [¬(Hydrophobic(U))∨Hydrophobicity(U)+] + [¬(F(U))∨Hydrophobic(skf0197(U))+] --> Set([¬(F(skf0197(U_192)))∨Hydrophobicity(skf0197(U_192))])") {
      // [¬(Hydrophobic(U))∨Hydrophobicity(U)+] + [¬(F(U))∨Hydrophobic(skf0197(U))+]
      // --> Set([¬(F(skf0197(U_192)))∨Hydrophobicity(skf0197(U_192))])

      // should be Derived: 104[0:Res:28.1,4.0] || F(U) -> Hydrophobicity(skf0_197(U))*.

      val u = Variable("U")
      val clause1 = StandardClause(Negation(Predicate("Hydrophobic", u)), Predicate("Hydrophobicity", u))
      val clause2 = StandardClause(Negation(Predicate("F", u)), Predicate("Hydrophobic", Function("skf0197", u)))
      val resolvent = StandardClause(Negation(Predicate("F", u)), Predicate("Hydrophobicity", Function("skf0197", u)))

      resolver.resolve(clause1, clause2) should equal(Set(resolvent))


    }


  }
}