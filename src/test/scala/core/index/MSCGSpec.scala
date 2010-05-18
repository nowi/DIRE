package core.index

/**
 * User: nowi
 * Date: 14.04.2010
 * Time: 15:23:49
 */

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import domain.fol.Substitution
import helpers.Logging
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class MSCGSpec extends Spec with ShouldMatchers with Logging {
  describe("MSCG transition system") {
    val y = Variable("y")
    val x = Variable("x")
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    val x3 = Variable("x3")
    val a = Constant("a")
    val b = Constant("b")
    val c = Constant("c")
    val g = (node: FOLNode) => Function("g", node)

    it("ARG transitions should work") {
      val h1 = Function("h", Constant("a"), IndicatorVariable(2), Variable("x1"), IndicatorVariable(1), IndicatorVariable(1))
      val h2 = Function("h", Constant("a"), IndicatorVariable(2), Constant("c"), Constant("b"), Constant("b"))

      LMSCG(h1, h2, Substitution(), Substitution(),Nil) should not equal (None)


    }



    it("should perform the corrct steps on the example from p.178 , term indexing , peter graf") {
      val r: Substitution = Substitution(Map(y -> a, x1 -> a, x2 -> b, x3 -> Function("g", c)))
      val p: Substitution = Substitution(Map(x1 -> a, x2 -> c, x3 -> Function("g", a)))
      val result = MSCG(r, p, Substitution(), Substitution(), Substitution(),Nil)
      log.info("MSCG of %s and %s was : %s" format (r, p, result))
      result should not equal (None)

    }

    it("should return a nom empty mscg for r = {x1 -> *1, x3 -> b} and {x -> f(x1,x2) }   p = {x -> f(c,g(d))} ") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val rfalsch = Map(x1 -> f(x1, x2))




      val r :  Substitution = Map(x -> f(x1, x2))

      val childSub : Substitution= Map(x1 -> IndicatorVariable(1),x3->b)

      val p: Substitution = Map(x -> f(c, g(Constant("d"))))

      val result = LMSCG((r join childSub), p,Nil)
      log.info("MSCG of %s and %s was : %s" format (r, p, result))
      (result match {
        case Some((g, s1, s2)) if (!g.isEmpty) => true
        case _ => false
      }) should be(true)


      // first try without applying p to r  --> should not return nonempty mscg
      (LMSCG(rfalsch, p,Nil) match {
        case Some((g, s1, s2)) if (g.isEmpty) => true
        case _ => false
      }) should be(true)


    }


    it("should return a empty mscg for r = {x1 -> *1, x3 -> b} and {x2 -> g(x3) }   p = {x -> f(b,g(a))} ") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      //val r = Map(x1 -> IndicatorVariable(1),x2 -> b )
      val r : Substitution  = Map(x -> g(x3))


      val childSub : Substitution = Map(x1 -> IndicatorVariable(1),x3->b)


      val p: Substitution = Map(x -> f(b, g(Constant("a"))))

      val result = LMSCG((r join childSub) , p,Nil)
      log.info("MSCG of %s and %s was : %s" format (r, p, result))
      (result match {
        case Some((g, s1, s2)) if (g.isEmpty) => true
        case _ => false
      }) should be(true)

    }

    it("UNDEFINED should return a empty mscg for r = {x1 -> *1, x3 -> b} and {x2 -> g(x3) }   p = {x -> f(b,g(a))} ") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      //val r = Map(x1 -> IndicatorVariable(1),x2 -> b )
      val r : Substitution  = Map(Variable("x_40") -> g(Variable("x_200")))

      val childSub : Substitution = Map(Variable("x_38") -> IndicatorVariable(0),Variable("x_200")->b)



      //Map(x -> f(b,g(a)))
      val p: Substitution = Map(x -> f(b, g(Constant("a"))), Variable("x_38") -> b , Variable("x_40") -> g(a) )

      val result = LMSCG((r join childSub) , p.toList.head,Nil )
      log.info("MSCG of %s and %s was : %s" format (r, p, result))
      (result match {
        case Some((g, s1, s2)) if (g.isEmpty) => true
        case _ => false
      }) should be(true)

    }


   

  }


}