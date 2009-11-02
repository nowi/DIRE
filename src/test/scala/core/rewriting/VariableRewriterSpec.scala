package core.rewriting

/**
 * User: nowi
 * Date: 01.11.2009
 * Time: 15:44:57
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class VariableRewriterSpec extends Spec with ShouldMatchers {
  describe("A VariableRewriter") {
    //    it("should rewriteClause VAriables nested in Prediacates") {
    //      // init with the resolution example from the AIMA Book page 298
    //
    //      val rewriter = ( new Object with VariableRewriting)
    //
    //
    //      val x = Variable("x")
    //      val y = Variable("y")
    //      val z = Variable("z1")
    //      val z1 = Variable("z2")
    //
    //      // replace x with z1 , and y with z2
    //
    //      val theta = Map(x -> z, y -> z1)
    //
    //      val p1 = Predicate("Knows", x)
    //      val p2 = Predicate("Knows", y)
    //      val p3 = Predicate("Knows", x, y)
    //
    //      val r1 = Predicate("Knows", z)
    //      val r2 = Predicate("Knows", z1)
    //      val r3 = Predicate("Knows", z, z1)
    //
    //      rewriter.rewrite(p1, theta) should equal(r1)
    //      rewriter.rewrite(p2, theta) should equal(r2)
    //      rewriter.rewrite(p3, theta) should equal(r3)
    //
    //      // should work in negation to
    //
    //      val np1 = Negation(p1)
    //      val np2 = Negation(p2)
    //      val np3 = Negation(p3)
    //
    //      val nr1 = Negation(r1)
    //      val nr2 = Negation(r2)
    //      val nr3 = Negation(r3)
    //
    //      rewriter.rewrite(np1, theta) should equal(nr1)
    //      rewriter.rewrite(np2, theta) should equal(nr2)
    //      rewriter.rewrite(np3, theta) should equal(nr3)
    //
    //
    //    }


    it("should rewrite Loves(G(List(x)),x) to Loves(G(List(Jack)),Jack)") {
      // init with the resolution example from the AIMA Book page 298

      val rewriter = new VariableRewriter()


      val x = Variable("x")
      val jack = Constant("Jack")

      // replace x with z1 , and y with z2

      val theta = Map(x -> jack)

      val p1 = Predicate("Loves", Function("G", x), x)
      val r1 = Predicate("Loves", Function("G", jack), jack)

      rewriter.rewrite(p1, theta) should equal(r1)

      // should work in negation to

      val np1 = Negation(p1)

      val nr1 = Negation(r1)

      rewriter.rewrite(np1, theta) should equal(nr1)


    }


  }
}