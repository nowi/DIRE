package core.reduction

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 11:56:33
 */

import containers.{CNFClauseStore}
import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.jteigen.scalatest.JUnit4Runner

@RunWith(classOf[JUnit4Runner])
abstract class ClauseTautologyDetectionSpec extends Spec with ShouldMatchers with Logging {
  val tautologyDetector : ClauseTautologyDetection

  describe("A SYNTACTIC clause level tautology detector should") {
    val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val m2 = Constant("M2")
      val america = Constant("America")
                                val g = (node: FOLNode) => Function("g", node)
    val f = (node1: FOLNode, node2: FOLNode) => Function("f", node1, node2)

    it("detect clauses with ground tautologuise ") {
      tautologyDetector(StandardClause(west,Negation(west))) should be (true)
      tautologyDetector(StandardClause(west,nono)) should be (false)
      tautologyDetector(StandardClause(west,nono,west)) should be (false)
    }


    it("find clauses with non ground tautologuise ") {
      tautologyDetector(StandardClause(g(x),Negation(g(x)))) should be (true)
      tautologyDetector(StandardClause(g(x),g(x))) should be (false)
      tautologyDetector(StandardClause(g(x),Negation(g(y)))) should be (false)
      tautologyDetector(StandardClause(g(x),g(x),Negation(g(y)))) should be (false)
    }




  }


}
