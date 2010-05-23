package domain.fol.parsers

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import ast._
import com.jteigen.scalatest.JUnit4Runner

import helpers.Logging
import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class FOLDomainSpec extends Spec with ShouldMatchers with Logging {
  describe("The FOLDomain Objects") {
    it("literals should be only instantiable with atomic sentences and throw class cast exception if not") {
      val richard = Constant("richard");
      val john = Constant("john");

      // create literals
      val richardAndJohn = AndConnective(richard, john);

      // should not qualify as literal

      assert(richardAndJohn match {
        case FOLLiteral(x) => false
        case _ => true
      })

      assert(true)

    }


    it("clausel should be constructable from literals") {
      val richard = Constant("richard");
      val john = Constant("john");

      val clause = StandardClause(richard, john);

      println(clause)

      assert(true)

    }

    it("clause should be able to return its positive/negative literals") {
      val richard = Constant("richard");
      val john = Negation(Constant("john"));

      // create literals

      val clause = StandardClause(richard, john)

      val positives = clause.positiveLiterals;
      log.trace("Positive Literals of CLause %s are %s", clause, positives)

      val negatives = clause.negativeLiterals;
      log.trace("Negative Literals of CLause %s are %s", clause, negatives)

      // should contain richard
      assert(positives contains richard);
      assert(!(positives contains john));

      assert(negatives contains john);
      assert(!(negatives contains richard));

      assert(true);

    }


    it("a function should be able to return its nested variables") {
      // Loves(SF1(v2),v2)
      // Loves(v3,SF0(v3))
      // or
      // P(v1,SF0(v1),SF0(v1))
      // P(v2,SF0(v2),v2     )
      // or
      // P(v1,   F(v2),F(v2),F(v2),v1,      F(F(v1)),F(F(F(v1))),v2)
      // P(F(v3),v4,   v5,   v6,   F(F(v5)),v4,      F(v3),      F(F(v5)))

      val v2 = Variable("v2")
      val SF1 = Function("SF1", List(v2));
      val Loves = Function("Loves", List(SF1, v2));


      // get Variables from loves
      val lovesvars = Loves.vars;
      Loves.printVars
      Loves.printFlatArgs

      assert(Loves.vars contains v2);
      assert(Loves.vars.size == 2);


    }


//    it("a clause should be able to return its signature ( name-arity pairs)") {
//      val x = Variable("x")
//      val y = Variable("y")
//      val z = Variable("z")
//      val west = Constant("West")
//      val nono = Constant("Nono")
//      val m1 = Constant("M1")
//      val america = Constant("America")
//      val sells = (x: FOLNode, y: FOLNode, z: FOLNode) => Predicate("Sells", x, y, z)
//      val weapon = (x: FOLNode) => Predicate("Weapon", x)
//      val american = (x: FOLNode) => Predicate("American", x)
//      val hostile = (x: FOLNode) => Predicate("Hostile", x)
//      val missile = (x: FOLNode) => Predicate("Missile", x)
//      val owns = (x: FOLNode, y: FOLNode) => Predicate("Owns", x, y)
//      val enemy = (x: FOLNode, y: FOLNode) => Predicate("Enemy", x, y)
//
//
//      val C1 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
//        Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
//        Predicate("Criminal", x))
//
//
//      val C2 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
//        Negation(Predicate("Sells", x, y, z)),
//        Predicate("Criminal", x))
//
//
//      val signatureC1 = C1.signature
//      log.debug("Signature of Clause C1 %s is : %s", C1, signatureC1)
//
//      val signatureC2 = C2.signature
//      log.debug("Signature of Clause C2 %s is : %s", C2, signatureC2)
//
//      // c2sig should be subset
//      signatureC1.contains(signatureC2) should be(true)
//
//
//
//
//
//
//
//      val C3 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
//        Predicate("Sells", x, y, z), Negation(Predicate("Hostile", z)),
//        Predicate("Criminal", x))
//
//
//      val C4 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
//        Negation(Predicate("Sells", x, y, z)),
//        Predicate("Criminal", x))
//
//
//      val signatureC3 = C3.signature
//      log.debug("Signature of Clause C3 %s is : %s", C3, signatureC3)
//
//      val signatureC4 = C4.signature
//      log.debug("Signature of Clause C4 %s is : %s", C4, signatureC4)
//
//
//      // same length but difference in sig
//
//      // c2sig should be subset
//      signatureC3.contains(signatureC4) should be(false)
//
//
//    }


  }

}
