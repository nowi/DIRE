package de.unima.dire.core.ordering

;

/**
 * User: nowi
 * Date: 19.01.2010
 * Time: 19:04:06
 */



import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
class LexiocgraphicPrecedenceSpec extends Spec with ShouldMatchers {
  describe("LexiocgraphicPrecedenceSpec") {


    // build the precedence
    lazy val precedence = LazyLexicographicPrecedence

    println("Precedence : " + precedence)

    it("NEWATOMIC10 > hasHydrophobicity") {
      val x = "NEWATOMIC10"
      val y = "hasHydrophobicity"
      precedence.compare(x, y) should equal(1)
    }

    it("hasHydrophobicity == hasHydrophobicity") {
      val x = "hasHydrophobicity"
      val y = "hasHydrophobicity"
      precedence.compare(x, y) should equal(0)
    }

    it("NEWATOMIC25 > NEWATOMIC23p") {
      val x = "NEWATOMIC25"
      val y = "NEWATOMIC23p"
      precedence.compare(x, y) should equal(1)
    }

    it("NEWATOMIC5 > NEWATOMIC26") {
      val x = "NEWATOMIC5"
      val y = "NEWATOMIC26"
      precedence.compare(x, y) should equal(1)
    }
  }


}
