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
class FOLDslSpec extends Spec with ShouldMatchers {
  describe("A FOLParser") {
    it("should parse simple term equality") {
      val text = "'richard' iff 'richard'"
      // parse
      assert(FOLDsl.parse(text))

    }

    it("should parse : Brother(Richard,John) and Brother(John, Richard)") {
      val text = "(Brother ('Richard','John') and Brother ('John', 'Richard'))"
      // parse
      assert(FOLDsl.parse(text))

    }

    it("should parse : not Brother(Richard,John)") {
      val text = "not(Brother ('Richard','John'))"
      // parse
      assert(FOLDsl.parse(text))

    }


    it("should parse Older(John, 30) => not Younger(John, 30)") {
      val text = "(Older('John','20') then not (Younger('John','20')))"
      // parse
      assert(FOLDsl.parse(text))

    }


    it("should parse For all x Cat(x) => Mammal(x) ") {
      val text = "(forall x Cat(x) then Mammal(x))"
      // parse
      assert(FOLDsl.parse(text))

    }

    it("should parse exist Sister(x, Spot) A Cat(x)  ") {
      val text = "(exists x Sister(x,'Spot') then Cat(x))"
      // parse
      assert(FOLDsl.parse(text))

    }

    it("should parse ∀x : Set(x) → ∃y : part(x, y) ∧ Set(y).") {
      val text = "(forall x Set(x) then exists y (part(x,y) and Set(y)))"
      // parse
      assert(FOLDsl.parse(text))

    }
  }

}
