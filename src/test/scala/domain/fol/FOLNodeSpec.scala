package domain.fol

/**
 * User: nowi
 * Date: 11.04.2010
 * Time: 14:14:21
 */
import ast._
import com.jteigen.scalatest.JUnit4Runner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class FOLNodeSpec extends Spec with ShouldMatchers {
  val funH = Function("h", Constant("a"), Function("g", Constant("a")), Variable("x"))
  val funX = Function("f", Variable("x"))
  val funY = Function("f", Variable("y"))
  describe("FOLNode") {

    it("should be a value type") {
      val f1 = Function("h", Constant("a"), Function("g", Constant("a")), Variable("x"))
      val f2 = Function("h", Constant("a"), Function("g", Constant("a")), Variable("x"))

      println(f1)
      println(f2)
      // compare based on jvm reference identity
      (f1 eq f2) should be(true)

    }

//    it("should return all avaialiabe positions in the terms") {
//      funH.positions.flatten(itr => itr) should equal(List(0, 1, List(2), List(2, 1), 3))
//
//    }

    it("should give access to subterms by indexpath") {
      funH(List(0)) should equal(funH)
      funH(List(2)) should equal(Function("g", Constant("a")))
      funH(List(2, 1)) should equal(Constant("a"))
      funH(List(2, List(1))) should equal(Constant("a"))

    }

//    it("should give access to subterms by indexpath to all indexpaths returned by positions") {
//      funH.positions.flatten(itr => itr).foreach({pos: Any => funH(List(pos))})
//      true
//    }


    it("should normalize itself") {
      funX.normalize should equal(funY.normalize)
      (Function("f", Variable("x"), Variable("x"), Variable("y")).normalize == Function("f", Variable("z"), Variable("z"), Variable("x")).normalize) should be(true)
    }


    it("negation should normalize itself") {
      Negation(funX).normalize.asInstanceOf[Negation].filler should equal(funY.normalize)
    } 



    it("should NOT rewrite with a EMPTY substitution") {
      val term = Function("f", Variable("x"), Variable("x"), Variable("y"))
      (term.rewrite(Substitution())) should equal(term)
    }


  }


}