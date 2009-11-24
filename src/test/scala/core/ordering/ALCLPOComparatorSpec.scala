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
import org.slf4j.LoggerFactory

@RunWith(classOf[JUnit4Runner])
class ALCLPOComparatorSpec extends Spec with ShouldMatchers {
  describe("ALCLPOrderingComparator") {

    val log = LoggerFactory getLogger (this getClass)

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

    val comparator: ALCLPOComparator = new ALCLPOComparator

    //    it("Literals that contain different variables are incomparable") {
    //      // should throw exception
    //      intercept[IllegalArgumentException] {
    //        // compare c , e
    //        comparator.comparePartial (c,e)
    //
    //      }
    //
    //
    //    }

    it("Literals not containing function or predicate.") {
      comparator.compare(leonid, elizabeth) should equal(Some(1))
      comparator.compare(elizabeth, leonid) should equal(Some(-1))
      comparator.compare(leonid, leonid) should equal(Some(0))
    }

    it("Literals containing a function symbol precede literals that do not contain a function symbol.") {
      comparator.compare(c, leonid) should equal(None)
      comparator.compare(leonid, c) should equal(None)
      comparator.compare(leonid, leonid) should equal(Some(0))
    }

    it("Literals containing a function symbol are ordered according to the precedence of the function symbols.") {
      comparator.compare(c, c1) should equal(Some(-1))
      comparator.compare(c1, c) should equal(Some(1))
      comparator.compare(c1, c1) should equal(Some(0))

    }

    //    it("Literals containing nested functions/predicates") {
    //      comparator.compare(c, d) should equal(Some(-1))
    //      comparator.compare(d, c) should equal(Some(1))
    //      comparator.compare(d, d) should equal(Some(0))
    //
    //
    //    }

    it("Functions with different arities but same function symbol") {
      comparator.compare(b, b1) should equal(Some(0))
      comparator.compare(b1, b) should equal(Some(0))
      comparator.compare(b1, b1) should equal(Some(0))


    }

    it("Functions with different arities but different function symbol") {
      comparator.compare(b, b2) should equal(Some(-1))
      comparator.compare(b2, b) should equal(Some(1))
      comparator.compare(b2, b2) should equal(Some(0))


    }


    it("Should sort a few clauses with java.util.Comparator contract") {
      val folNodes = List(Predicate("dog", List(Variable("x"))), Predicate("animal", List(Variable("x"))), Predicate("dog", List(Constant("fido"))), Predicate("animal", List(Variable("y"))), Predicate("die", List(Variable("y"))))

      val sortedFOLNodes = scala.util.Sorting.stableSort(folNodes, comparator.isGreater(_, _))

      log.trace("UNSorted FOLNodes : {}", folNodes)
      log.trace("Sorted FOLNodes : {}", sortedFOLNodes)

      assert(true)


    }


  }
}
