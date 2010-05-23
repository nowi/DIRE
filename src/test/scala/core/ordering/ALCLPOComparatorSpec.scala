package core.ordering

/**
 * User: nowi
 * Date: 30.10.2009
 * Time: 13:41:15
 */

import caches.MaxLitCache
import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import helpers.Logging


@RunWith(classOf[JUnit4Runner])
class ALCLPOComparatorSpec extends Spec with ShouldMatchers with Logging {
  describe("ALCLPOrderingComparator") {

    // craete adhoc configration
    val f = (term: FOLNode) => Function("f", term)
    val g = (term: FOLNode) => Function("g", term)
    val x = Variable("x")
    val a = Constant("a")





//    it(" (This implies that R(x,f(x)) ≻ ¬C(x) and D(f(x)) ≻ ¬C(x), for all function symbols f, and predicates R,C, and D.)") {
//      val config = new Object {
//        // the initial clause store
//        // ordered resolution needs comparator and selection too
//        lazy val precedence = LazyLexicographicPrecedence
//      }
//
//      val comperator: ALCLPOComparator = new ALCLPOComparator(config)
//      val R = Predicate("R", x, f(x))
//      val negC = Negation(Predicate("C", x))
//      val D = Predicate("D", f(x))
//
//      comperator.compare(R, negC) should be(Some(1))
//      comperator.compare(D, negC) should be(Some(1))
//
//    }
//
//    it("Literals of type (¬)P(f(x)), are larger than any literal of type P(x) independent of substitution by (iii) because f(x)σ ≽ Q(x)σ") {
//      val config = new Object {
//        // the initial clause store
//        // ordered resolution needs comparator and selection too
//        lazy val precedence = LazyLexicographicPrecedence
//      }
//      val precedence = config.precedence
//      val comperator: ALCLPOComparator = new ALCLPOComparator(config)
//      val negC = Negation(Predicate("C", x))
//      val D = Predicate("D", f(x))
//      comperator.compare(Predicate("P", f(x)), Predicate("P", x)) should be(Some(1))
//      comperator.compare(Predicate("P", f(a)), Predicate("P", a)) should be(Some(1))
//      comperator.compare(Negation(Predicate("P", f(x))), Predicate("P", x)) should be(Some(1))
//      comperator.compare(Negation(Predicate("P", f(a))), Predicate("P", a)) should be(Some(1))
//    }
//
//
//    it("P(x)σ ≻ Q(x)σ iff P > Q") {
//      val config = new Object {
//        // the initial clause store
//        // ordered resolution needs comparator and selection too
//        lazy val precedence = LazyLexicographicPrecedence
//      }
//
//      val comperator: ALCLPOComparator = new ALCLPOComparator(config)
//      val precedence = config.precedence
//
//      // first check precedence , should be P > Q in this case
//      precedence.compare("P","Q") < 0 should be(true)
//
//      comperator.compare(Predicate("P", x), Predicate("Q", x)) should be(Some(-1))
//      comperator.compare(Predicate("P", a), Predicate("Q", a)) should be(Some(-1))
//
//
//
//
//    }
//
//    it("P(f(x))σ ≻ Q(g(x))σ iff (P > Q and f > g )") {
//      val config = new Object {
//        // the initial clause store
//        // ordered resolution needs comparator and selection too
//        lazy val precedence = LazyLexicographicPrecedence
//      }
//
//      val comperator: ALCLPOComparator = new ALCLPOComparator(config)
//      val precedence = config.precedence
//
//      // first check precedence , should be P > Q in this case
//      precedence.compare("P","Q") < 0 should be(true)
//      (precedence.compare("f","g") < 0) should be(true)
//
//      comperator.compare(Predicate("P", f(x)), Predicate("Q", g(x))) should be(Some(-1))
//      comperator.compare(Predicate("P", f(a)), Predicate("Q", g(a))) should be(Some(-1))
//
//    }

//    it("P(f(x))σ ≻ Q(g(x))σ iff (P > Q and f = g )") {
//      val config = new Object {
//        // the initial clause store
//        // ordered resolution needs comparator and selection too
//        lazy val precedence = LazyLexicographicPrecedence
//      }
//
//      val comperator: ALCLPOComparator = new ALCLPOComparator(config)
//      val precedence = config.precedence
//
//      // first check precedence , should be P > Q in this case
//      precedence.compare("P","Q") < 0 should be(true)
//      (precedence.compare("f","f") == 0) should be(true)
//
//      comperator.compare(Predicate("P", f(x)), Predicate("Q", f(x))) should be(Some(-1))
//      comperator.compare(Predicate("P", f(a)), Predicate("Q", f(a))) should be(Some(-1))
//
//    }

    it("Check maxLits in context of conference ontology") {
      val config = new Object {
        // the initial clause store
        // ordered resolution needs comparator and selection too
        lazy val precedence = new core.ordering.CustomConferencePartitionedPrecedence
      }

      implicit val comperator: ALCLPOComparator = new ALCLPOComparator(config)
      implicit val maxLitCache: MaxLitCache = new MaxLitCache()
      val precedence = config.precedence

      val u = Variable("U")
      val a = ALCDClause(Negation(Predicate("O1Author",u)),Predicate("O0Paper",Function("skf135",u)))
      val b = ALCDClause(Negation(Predicate("O0Paper",u)),Predicate("O0readByReviewer",u,Function("skf08",u)))

      val bMaxLits = b.maxLits

      log.info("BMaxlits : %s",bMaxLits)
      true
    }


  }
}
