package core.ordering

;

/**
 * User: nowi
 * Date: 19.01.2010
 * Time: 19:04:06
 */
import com.jteigen.scalatest.JUnit4Runner

import config.Partition1OrderedTheoremProvingConfig
import containers.{CNFClauseStore}

import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
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
