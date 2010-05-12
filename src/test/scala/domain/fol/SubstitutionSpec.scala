package domain.fol

/**
 * User: nowi
 * Date: 13.04.2010
 * Time: 11:56:04
 */

import ast._
import com.jteigen.scalatest.JUnit4Runner
import helpers.Logging
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class SubstitutionSpec extends Spec with ShouldMatchers with Logging{
  describe("Substitution") {
    val f1 = Function("f", Variable("u"), Variable("v"))
    val f2 = Function("f", Constant("a"), Variable("v"))



    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    val a = Constant("a")
    val b = Constant("b")
    val c = Constant("c")

    it("should normalize itself") {
      val s: Substitution = Map(x -> f1, y -> f2)

      val ns: Substitution = s.normalize


      val f1n = Function("f", IndicatorVariable(0), IndicatorVariable(1))
      val f2n = Function("f", Constant("a"), IndicatorVariable(1))
      ns should equal(Map(x -> f1n, y -> f2n))


    }

    it("should return its image") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val s: Substitution = Map(x -> f(a,b), y -> g(z))

      s.image should equal(List(z))


    }


    it("two substitutions should be composable") {
      val s: Substitution = Map(z -> Function("f", x))
      val r: Substitution = Map(x -> a, y -> c)
      val sr: Substitution = Map(z -> Function("f", a), x -> a, y -> c)
      val composed = (s compose r)
      composed should equal(sr)
      // antysymetric
      composed should not equal (r compose s)

    }


    it("two substitutions should be joinable") {
      val s: Substitution = Map(z -> Function("f", x))
      val r: Substitution = Map(x -> a, y -> c)
      val sr: Substitution = Map(z -> Function("f", a), y -> c)
      val joined = (s join r)
      joined should equal(sr)

      // antysymetric
      joined should not equal (r join s)

    }

    it("two substitutions should be joinable 2") {
      val s: Substitution = Map(z -> Function("f", x,y))
      val r: Substitution = Map(x -> a, y -> c)
      val sr: Substitution = Map(z -> Function("f",a,c))
      val joined = (s join r)
      joined should equal(sr)

      // antysymetric
      joined should not equal (r join s)


    }






    it("substitution should be restrictable") {
      val r: Substitution = Map(x -> a, y -> c)
      val u = List(y)
      val restricted = r.restrict(u)
      restricted should equal(Map(y -> c))
    }

    it("(x -> f(x_38,x_40)) is a generalization of (x -> f(c,g(d)))") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val r: Substitution = Map(x -> f(Variable(), Variable()))
      val p: Substitution = Map(x -> f(c, g(Constant("d"))))

      Substitution.generalizations(r, p).isEmpty should be(false)
    }
    it("(x -> f(x_38,x_40)) is a variation of (x -> f(c,g(d)))") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val r: Substitution = Map(x -> f(Variable(), Variable()))
      val p: Substitution = Map(x -> f(c, g(Constant("d"))))

      Substitution.variants(r, p).isEmpty should be(false)
    }


    it("((x -> f(x1,x2)) is a NOT A generalization (x1 -> *1,x2 -> g(b))  )") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val general: Substitution = Map(x -> f(Variable("x1"), Variable("x2")))
      val instance: Substitution = Map(Variable("x2") -> IndicatorVariable(1),Variable("x3") -> Constant("b"))

      // should find empty generalizer
      Substitution.generalizations(instance,general).isDefined should be(false)
    }

    it("((x -> f(x1,x2)) is NOT A generalization (x2 -> g(x3))  )") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val general: Substitution = Map(x -> f(Variable("x1"), Variable("x2")))
      val instance: Substitution = Map(Variable("x2") -> g(Variable("x3")))

      // should find empty generalizer
      Substitution.generalizations(general,instance).isDefined should be(false)
      //Substitution.generalizations(instance,general).get.isEmpty should be(true)
    }

    // TODO check naming  variant != variation

    it("( (x -> f(c,g(d))) is a not variannt of  ( x -> f(x_38,x_40)) normalized !") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val r: Substitution = Map(x -> f(Variable(), Variable()))
      val p: Substitution = Map(x -> f(c, g(Constant("d"))))

      // if r is normalized this fails because V* intersect domain(variantMatcher) == notempty
      Substitution.variants(r.normalize, p.normalize).isEmpty should be(true)
    }


    it("( (x -> f(c,g(d))) is a variannt of  x -> f(x_38,x_40))") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val r: Substitution = Map(x -> f(Variable(), Variable()))
      val p: Substitution = Map(x -> f(c, g(Constant("d"))))

      Substitution.variants(r, p.normalize).isEmpty should be(false)
    }


    it("( x2 -> g(x3)) is a variannt subnode of x -> f(x_38,x_40)) under substitution (x -> f(b,g(a)))") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val r1 = Map(x -> f(Variable("x1"),Variable("x2")))
      val p: Substitution = Map(x -> f(b, g(a)))


      val pSoFar = (p compose Substitution.variants(r1, p.normalize).get)
      val r2: Substitution = Map(Variable("x2") -> g(Variable("x3")))

      (Substitution.variants(r2, pSoFar) match {
        case Some(vs) => {
          log.info("Variant matcher is %s" format (vs))
          log.info("p2 was %s" format (pSoFar))
          log.info("r2 was %s" format (r2))
          true
        }
        case None => false
      }) should be (true)
    }


    it("( x1 -> *1 , x2 -> g(b) ) is a NOT variannt subnode ? of x -> f(x_38,x_40)) under substitution (x -> f(c,g(d)))") {
      val f = (t1: FOLNode, t2: FOLNode) => Function("f", t1, t2)
      val g = (t1: FOLNode) => Function("g", t1)

      val r1 = Map(x -> f(Variable("x1"),Variable("x2")))
      val p: Substitution = Map(x -> f(c, g(Constant("d"))))


      val pSoFar = (p compose Substitution.variants(r1, p.normalize).get)
      val r2: Substitution = Map(Variable("x1") -> IndicatorVariable(1),Variable("x2") -> g(b))

      (Substitution.variants(r2, pSoFar) match {
        case Some(vs) => {
          log.info("Variant matcher is %s" format (vs))
          log.info("p2 was %s" format (pSoFar))
          log.info("r2 was %s" format (r2))
          true
        }
        case None => false
      }) should be (false)
    }


  }
}