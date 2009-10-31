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

@RunWith(classOf[JUnit4Runner])
class LiteralComparatorSpec extends Spec with ShouldMatchers {
  describe("ALCLPOrderingComparator") {
    val john = Constant("John")
    val jane = Constant("Jane")
    val leonid = Constant("Leonid")
    val elizabeth = Constant("Elizabeth")
    val a = Function("Knows", List(john, Variable("x")))
    val b = Function("Knows", List(john, jane))
    val b1 = Function("Knows", List(john))
    val c = Function("Knows", List(Variable("y"), leonid))
    val c1 = Function("Knows1", List(Variable("y"), leonid))
    val d = Function("Knows", List(Variable("y"), Function("Mother", List(Variable("y")))))
    val e = Function("Knows", List(Variable("x"), elizabeth))

    val comparator: ALCLPComparator = new ALCLPComparator

    //    it("Literals that contain different variables are incomparable") {
    //      // should throw exception
    //      intercept[IllegalArgumentException] {
    //        // compare c , e
    //        comparator.compare (c,e)
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
      comparator.compare(c, leonid) should equal(Some(1))
      comparator.compare(leonid, c) should equal(Some(-1))
      comparator.compare(leonid, leonid) should equal(Some(0))
    }

    it("Literals containing a function symbol are ordered according to the precedence of the function symbols.") {
      comparator.compare(c, c1) should equal(Some(-1))
      comparator.compare(c1, c) should equal(Some(1))
      comparator.compare(c1, c1) should equal(Some(0))

    }


    it("Literals containing nested functions/predicates") {
      comparator.compare(c, d) should equal(Some(-1))
      comparator.compare(d, c) should equal(Some(1))
      comparator.compare(d, d) should equal(Some(0))


    }

    it("Functions with different arities") {
      comparator.compare(b, b1) should equal(Some(1))
      comparator.compare(b1, b) should equal(Some(-1))
      comparator.compare(b1, b1) should equal(Some(0))


    }


  }
}
