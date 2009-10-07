package domain.fol.parsers

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import com.jteigen.scalatest.JUnit4Runner

import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class FOLDomainSpec extends Spec with ShouldMatchers {
  describe("The FOLDomain Objects") {
    it("literals should be only instantiable with atomic sentences and throw class cast exception if not") {
      val richard = Constant("richard");
      val john = Constant("john");

      // create literals
      val r = PositiveFOLLiteral(richard);
      val j = NegativeFOLLiteral(john);

      val richardAndJohn = AndConnective(richard, john);

      // should throw exception
      intercept[ClassCastException] {
        PositiveFOLLiteral(richardAndJohn);

      }


      assert(true)

    }


    it("clausel should be constructable from literals") {
      val richard = Constant("richard");
      val john = Constant("john");

      // create literals
      val r = PositiveFOLLiteral(richard);
      val j = NegativeFOLLiteral(john);

      val clause = Clause(Set(r, j))

      println(clause)

      assert(true)

    }

    it("clause should be able to return its positive/negative literals") {
      val richard = Constant("richard");
      val john = Constant("john");

      // create literals
      val r = PositiveFOLLiteral(richard);
      val j = NegativeFOLLiteral(john);

      val clause = Clause(Set(r, j))

      val positives = clause.positiveLiterals;
      val negatives = clause.negativeLiterals;

      // should contain richard
      assert(positives contains r);
      assert(!(positives contains j));

      assert(negatives contains j);
      assert(!(negatives contains r));

      assert(true);

    }


  }

}
