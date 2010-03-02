package core.ordering

;

/**
 * User: nowi
 * Date: 19.01.2010
 * Time: 19:04:06
 */
import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.parsers.SPASSIntermediateFormatParser;
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class CustomSPASSModule1PrecedenceSpec extends Spec with ShouldMatchers {
  describe("CustomSPASSModule1PrecedenceSpec") {

    // load the clauses
    val lines = scala.io.Source.fromFile("input/partitioned1clauses.spass").mkString
    val text: String = lines // parse
    val clauses = SPASSIntermediateFormatParser.parseClauseStore(text).getOrElse(CNFClauseStore())

    // build the precedence
    val precedence = new CustomConferencePartitionedPrecedence

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
