package core.containers

/**
 * User: nowi
 * Date: 17.12.2009
 * Time: 13:12:28
 */
import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import heuristics.LightestClauseHeuristicStorage
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
abstract class UsableClausesStoreSpec extends org.scalatest.Spec with ShouldMatchers {
  // test clauses
  def createStorage: LightestClauseHeuristicStorage

  describe("A UsableClausesStore with LightestClauseHeuristic") {
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


    val C1 = StandardClause( // size 4
      Negation(Predicate("American", x)),
      Negation(Predicate("Weapon", y)),
      Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
      Predicate("Criminal", x))

    val C2 = StandardClause( // size 3
      Negation(Predicate("Missile", x)),
      Negation(Predicate("Owns", nono, x)),
      Predicate("Sells", west, x, nono)
      )

    val C3 = StandardClause( // size 2
      Negation(Predicate("Enemy", x, america)),
      Predicate("Hostile", x)
      )


    val C4 = StandardClause( // size 2
      Negation(Predicate("Missile", x)),
      Predicate("Weapon", x)
      )

    val C5 = StandardClause( // size 1
      Predicate("Owns", nono, m1)
      )
    val C6 = StandardClause( // size 1
      Predicate("Missile", m1)
      )
    val C7 = StandardClause( // size 1
      Predicate("American", west)
      )

    val C8 = StandardClause( // size 1
      Predicate("Enemy", nono, america)
      )

    val goalClause = StandardClause( // size 1
      Negation(Predicate("Criminal", west))
      )

    it("should enque clauses , and maintian the order") {
      val storage = createStorage

      storage.addAll(List(C1, C2, C3, C4, C5, C6, C7, C8, goalClause))

      // check if they are dequeen in the right order
      // first all with size 1 in the order of insertion

      storage.removeNext should equal(C5)
      storage.removeNext should equal(C6)
      storage.removeNext should equal(C7)
      storage.removeNext should equal(C8)
      storage.removeNext should equal(goalClause)

      // size 2
      storage.removeNext should equal(C3)
      storage.removeNext should equal(C4)

      // size 3
      storage.removeNext should equal(C2)
      // size 4
      storage.removeNext should equal(C1)

    }
//
    it("should enque clauses , and be empty after dequeing") {
      val storage = createStorage
      storage.addAll(List(C1, C2, C3, C4, C5, C6, C7, C8, goalClause))

      // check if they are dequeen in the right order
      // first all with size 1 in the order of insertion

      storage.removeNext should equal(C5)
      storage.removeNext should equal(C6)
      storage.removeNext should equal(C7)
      storage.removeNext should equal(C8)
      storage.removeNext should equal(goalClause)

      // size 2
      storage.removeNext should equal(C3)
      storage.removeNext should equal(C4)

      // size 3
      storage.removeNext should equal(C2)
      // size 4
      storage.removeNext should equal(C1)

      storage.isEmpty should be(true)

    }
//
    it("should enque clauses , dequee , and enque interleaved claues correctyl") {
      val storage = createStorage
      storage.addAll(List(C1, C2, C3, C4, C5, C6, C7, C8, goalClause))

      // check if they are dequeen in the right order
      // first all with size 1 in the order of insertion

      storage.removeNext should equal(C5)
      storage.removeNext should equal(C6)
      storage.removeNext should equal(C7)
      storage.removeNext should equal(C8)

      storage.add(C8)
//      storage.add(C3)

      storage.removeNext should equal(goalClause)
      storage.removeNext should equal(C8)

      // size 2
      storage.removeNext should equal(C3)
      storage.removeNext should equal(C4)
//      storage.removeNext should equal(C3)

      // size 3
      storage.removeNext should equal(C2)
      // size 4
      storage.removeNext should equal(C1)

      storage.isEmpty should be(true)

    }

    it("should enque clauses , dequee , and enque interleaved claues correctyl, but fail on duplicate clause insertion") {
      val storage = createStorage
      storage.addAll(List(C1, C2, C3, C4, C5, C6, C7, C8, goalClause))

      // check if they are dequeen in the right order
      // first all with size 1 in the order of insertion

      storage.removeNext should equal(C5)
      storage.removeNext should equal(C6)
      storage.removeNext should equal(C7)
      storage.removeNext should equal(C8)

      storage.add(C8)


      storage.add(C3)

      

      storage.removeNext should equal(goalClause)
      storage.removeNext should equal(C8)

      // size 2
      storage.removeNext should equal(C3)
      storage.removeNext should equal(C4)
//      storage.removeNext should equal(C3)

      // size 3
      storage.removeNext should equal(C2)
      // size 4
      storage.removeNext should equal(C1)

      storage.isEmpty should be(true)

    }
//
//
//    it("should index the classes correctly and support retrieval") {
//      val storage = createStorage
//      storage.addAll(List(C1, C2, C3, C4, C5, C6, C7, C8, goalClause))
//      // get clauses for key :
//      storage.toList.isEmpty should be(true)
//
//    }


  }
}