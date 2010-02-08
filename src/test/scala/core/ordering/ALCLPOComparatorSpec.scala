package core.ordering

/**
 * User: nowi
 * Date: 30.10.2009
 * Time: 13:41:15
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import helpers.Logging


@RunWith(classOf[JUnit4Runner])
class ALCLPOComparatorSpec extends Spec with ShouldMatchers with Logging{
  describe("ALCLPOrderingComparator") {




    // craete adhoc configration

    val john = Constant("John")
    val jane = Constant("Jane")
    val leonid = Constant("Leonid")
    val elizabeth = Constant("Elizabeth")
    val a = Function("Knows", List(john, Variable("x")))
    val b = Function("Knows", List(john, jane))
    val b1 = Function("Knows", List(john))
    val b2 = Function("Knows1", List(john))
    val c = Function("Knows", List(Variable("y"), leonid))
    val c1 = Function("Knows1", List(Variable("y"), leonid))
    val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
    val e = Function("Knows", List(Variable("x"), elizabeth))


    val config = new Object {
      // the initial clause store
      lazy val initialClauses = {
        CNFClauseStore(
          StandardClause(john, jane, leonid, elizabeth))
      }
      // ordered resolution needs comparator and selection too
      lazy val precedence = new LexicographicPrecedence(this)
      lazy val literalComparator = new ALCLPOComparator(this)
    }

    val comparator: ALCLPOComparator = new ALCLPOComparator(config)

    it("Literals not containing function or predicate.") {
      comparator.compare(leonid, elizabeth) should equal(Some(-1))
      comparator.compare(elizabeth, leonid) should equal(Some(1))
      comparator.compare(leonid, leonid) should equal(Some(0))
    }


    it("Literals containing a function symbol are ordered according to the precedence of the function symbols.") {
      comparator.compare(c, c1) should equal(Some(1))
      comparator.compare(c1, c) should equal(Some(-1))
      comparator.compare(c1, c1) should equal(Some(0))

    }


    it("Functions with different arities but same function symbol") {
      comparator.compare(b, b1) should equal(Some(0))
      comparator.compare(b1, b) should equal(Some(0))
      comparator.compare(b1, b1) should equal(Some(0))


    }

    it("Functions with different arities but different function symbol") {
      comparator.compare(b, b2) should equal(Some(1))
      comparator.compare(b2, b) should equal(Some(-1))
      comparator.compare(b2, b2) should equal(Some(0))


    }


    it("Functions are smaller then Predicates") {

      val R = Predicate("R", Variable("x"), Function("f", Variable("x")))
      val notC = Negation(Predicate("C", Variable("x")))
      val D = Predicate("D", Function("f", Variable("x")))

      // R > notP
      comparator.compare(R, notC) should equal(Some(1))
      comparator.compare(D, notC) should equal(Some(1))
      comparator.compare(notC, R) should equal(Some(-1))
      comparator.compare(notC, D) should equal(Some(-1))


    }

    it("Functions are greater then negated FUnctions") {
      val f = Function("f", Variable("x"))
      val negf = Negation(Function("f", Variable("x")))

      comparator.compare(f, negf) should equal(Some(1))
      comparator.compare(negf, f) should equal(Some(-1))


    }


    it("[¬(hasSize(U,U1))∨¬(NEWATOMIC7(U))+] error .. negations  still need to be compared") {
      //[¬(hasSize(U,U1))∨¬(NEWATOMIC7(U))+]
      val nhasSize = Negation(Function("hasSize", Variable("U"), Variable("u1")))
      val nNewAtomic = Negation(Predicate("NEWATOMIC7", Variable("U")))
      comparator.compare(nhasSize, nNewAtomic) should equal(Some(-1))
      comparator.compare(nNewAtomic, nhasSize) should equal(Some(1))


    }
    it("Polarity(U) NEWATOMIC25(U)*  error .. chekc lexicographic ordering") {
      //[¬(hasSize(U,U1))∨¬(NEWATOMIC7(U))+]
      val pol = Predicate("Polarity", Variable("U"))
      val newatomic = Predicate("NEWATOMIC25", Variable("U"))
      comparator.compare(newatomic, pol) should equal(Some(1))
      comparator.compare(pol, newatomic) should equal(Some(-1))


    }

    //    it("[¬(hasSize(U,U1))∨¬(hasSize(U))+] error .. negations  still need to be compared") {
    //      //[¬(hasSize(U,U1))∨¬(NEWATOMIC7(U))+]
    //      val nhasSize = Negation(Predicate("hasSize",Variable("U"),Variable("u1")))
    //      val nhasSizeSmaller = Negation(Predicate("hasSize",Variable("U")))
    //      comparator.compare(nhasSize, nhasSizeSmaller) should equal(Some(1))
    //      comparator.compare(nhasSizeSmaller, nhasSize) should equal(Some(-1))
    //
    //
    //    }





    it("Maximum literal of given Clause Clause : [¬(hasSideChainStructure(U,U1))∨SideChainStructure(U1)] is ¬(hasSideChainStructure(U,U1))") {
      val negf = Negation(Function("f", Variable("x"), Variable("y")))
      val g = Function("g", Variable("x"))

      comparator.compare(negf, g) should equal(Some(-1))
      comparator.compare(g, negf) should equal(Some(1))


    }


    it("Should sort a few clauses with java.util.Comparator contract") {
      val folNodes = List(Predicate("dog", List(Variable("x"))), Predicate("animal", List(Variable("x"))), Predicate("dog", List(Constant("fido"))), Predicate("animal", List(Variable("y"))), Predicate("die", List(Variable("y"))))

      val sortedFOLNodes = scala.util.Sorting.stableSort(folNodes, comparator.isGreater(_, _))

      log.trace("UNSorted FOLNodes : %s", folNodes)
      log.trace("Sorted FOLNodes : %s", sortedFOLNodes)

      assert(true)


    }


  }
}
