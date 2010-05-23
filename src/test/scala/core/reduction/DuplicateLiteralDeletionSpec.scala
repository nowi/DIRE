package core.reduction

/**
 * User: nowi
 * Date: 29.04.2010
 * Time: 13:41:50
 */
import containers.{CNFClauseStore}
import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.jteigen.scalatest.JUnit4Runner

@RunWith(classOf[JUnit4Runner])
abstract class DuplicateLiteralDeletionSpec extends Spec with ShouldMatchers with Logging {
  val deleter: DuplicateLiteralDeletion

  describe("A DuplicateLiteralDeleter") {
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


    it("should delete duplicate ground literals. ") {
      deleter(Set(west,west,nono)) should equal (Set(west,nono))
    }


    it("should delete duplicate non ground literals. ") {
      deleter(Set(g(x),g(x),nono)) should equal (Set(g(x),nono))
      deleter(Set(g(x),g(y),nono)) should equal (Set(g(x),g(y),nono))
      deleter(Set(g(nono),g(nono),nono)) should equal (Set(g(nono),nono))
    }


  }


}