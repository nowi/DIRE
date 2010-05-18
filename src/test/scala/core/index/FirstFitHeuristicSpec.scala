package core.index

/**
 * User: nowi
 * Date: 17.04.2010
 * Time: 12:17:58
 */

import com.jteigen.scalatest.JUnit4Runner
import helpers.Logging
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.junit.runner.RunWith
import domain.fol.ast._

@RunWith(classOf[JUnit4Runner])
class FirstFitHeuristicSpec extends Spec with ShouldMatchers with Logging {
  describe("The heuristic uesed for SubstututionTreeIndex insertion") {
     val a = Constant("a")
    val b = Constant("b")
    val c = Constant("c")
    val d = Constant("d")
    val g = (node: FOLNode) => Function("g", node)
    val f = (node1: FOLNode, node2: FOLNode) => Function("f", node1, node2)
    val y = Variable("y")
    val z = Variable("z")

    val x = Variable("x")



    it("should first node yielding nonempty mscg for D should nbe (x2-> g(x3)"){
      val emtpyTree: SubstitutionIndexTree = EmptyTree()

      val A = emtpyTree.insert(f(z, g(b)))
      (A.substitution match {
        case Some(s) => {
          log.info("Substitution at A is %s", s)
          true
        }
        case _ => false

      }) should be(true)


      val B = A.insert(f(y, g(b)))
      (B match {
        case LeafNode(s,_,_) => {
          log.info("Substitution at B is %s", s)
          true
        }
        case _ => {
          log.info("B is %s", B)
          false
        }

      } ) should be(true)


      val C = B.insert(f(a, b))
      (C match {
        case InnerNode(s,children,_)  => {
          log.info("Substitution at C is %s", s)
          log.info("Children at C are %s", children)
          true
        }
        case _ => {
          log.info("C is %s", C)
          false
        }

      } ) should be(true)



      val D = C.insert(f(c, g(d)))
      (D match {
        case InnerNode(s,children,_)  => {
          log.info("Substitution at D is %s", s)
          log.info("Children at D are %s", children)
          true
        }
        case _ => {
          log.info("D is %s", D)
          false
        }

      } ) should be(true)


      true


    }




  }


}
