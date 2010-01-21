package domain.fol

;

/**
 * User: nowi
 * Date: 21.01.2010
 * Time: 13:48:59
 */

import ast.{Variable, StandardClause, Predicate, Negation}
import com.jteigen.scalatest.JUnit4Runner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class FOLClauseSpec extends Spec with ShouldMatchers {
  describe("FOLClause") {
    it("should correctly implement the equality trait")
              {
                val u = Variable("U")
                val a = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))
                val b = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))


                assert(a === b)


              }

    it("should correctly intersect sets ")
              {
                val u = Variable("U")
                val a = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))
                val b = StandardClause(Predicate("Tiny", u), Negation(Predicate("Small", u)))

                val interseect = Set(a, b) ** Set(b, a)
                assert(interseect === Set(a, b))


              }
  }

}
