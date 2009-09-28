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
    it("should parse simple constants") {
      val text = "('richard')"
      // parse
      FOLDsl.parse(text)

    }

    it("should parse : Brother(Richard,John) and Brother(John, Richard)") {
      val text = "(Brother ('Richard','John') and Brother ('John', 'Richard'))"
      // parse
      FOLDsl.parse(text)

    }

    it("should parse : not Brother(Richard,John)") {
      val text = "not(Brother ('Richard','John'))"
      // parse
      FOLDsl.parse(text)

    }


    it("Older(John, 30) => not Younger(John, 30)") {
      val text = "(Older('John','20') then not (Younger('John','20')))"
      // parse
      FOLDsl.parse(text)

    }
  }

}
