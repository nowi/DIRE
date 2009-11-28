package domain.fol.parsers

/**
 * User: nowi
 * Date: 28.11.2009
 * Time: 12:08:56
 */

import com.jteigen.scalatest.JUnit4Runner

import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class SPASSParserSpec extends Spec with ShouldMatchers {
  describe("A SPASSParser") {
    it("should parse example problem pelletier57 from spass spec 1.5") {
      val text: String = "beginproblem(Pelletier57). listofdescriptions. endoflist. listofsymbols. functions[(f,2), (a,0), (b,0), (c,0)]. predicates[(F,2)]. endoflist. listofformulae(axioms). formula(F(f(a,b),f(b,c))). formula(F(f(b,c),f(a,c))). formula(forall([U,V,W],implies(and(F(U,V),F(V,W)),F(U,W)))). endoflist. listofformulae(conjectures). formula(F(f(a,b),f(a,c))). endoflist. endproblem."
      // parse
      assert(SPASSParser.parse(text))

    }

  }

}
