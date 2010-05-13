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

    it("unshared types should be structural equal") {
      val f1 = Function("h", Constant("a"), Function("g", Constant("a")), Variable("x"))
      val f2 = Function("h", Constant("a"), Function("g", Constant("a")), Variable("x"))

      // compare pointer
      (f1 eq f2) should be(false)
      // compare structural
      (f1 == f2) should be(true)
    }

    it("shared nodes should be equal structurally and have same jvm object identity") {
       val f1s = FunctionS("h", ConstantS("a"), FunctionS("g", ConstantS("a")), VariableS("x"))
      val f2s = FunctionS("h", ConstantS("a"), FunctionS("g", ConstantS("a")), VariableS("x"))
      // compare pointer
      (f1s eq f2s) should be(true)
      // compare structure
      (f1s == f2s) should be(true)
    }


    it("shared function literals should correcly reference previous cached terms") {
      // first assume we have some shared terms In this adress space
      val a = ConstantS("a")
      val x = VariableS("x")
      val catS = FunctionS("cat",x,a)


      // now assume we have generated a TEMP Function same STRUCTURE
      val catT = Function("cat",Variable("x"),Constant("a"))

      // of course shared root term catT should be same as catS after intergration into shared space
      catT.shared eq catS should be (true)


      // assume we crate a new TEMP function , that cotains already shared terms but is not itself shared
      val catT2 = Function("cat2",Variable("x"),Constant("a"))

      // after integation the literals of this function sould point to the already shared terms


      // and the individual literals of the catT.shared should also be equal to the already existing
      // terms a and x
      catT2.args(0) == x should be (true)
      catT2.args(1) == a should be (true)

      catT2.args(0) eq x should be (false)
      catT2.args(1) eq a should be (false)

      catT2.shared.args(0) == x should be (true)
      catT2.shared.args(1) == a should be (true)

      catT2.shared.args(0) eq x should be (true)
      catT2.shared.args(1) eq a should be (true)

      
    }


    it("shared predicate literals should correcly reference previous cached terms") {
      // first assume we have some shared terms In this adress space
      val a = ConstantS("a")
      val x = VariableS("x")
      val catS = PredicateS("cat",x,a)


      // now assume we have generated a TEMP Function same STRUCTURE
      val catT = Predicate("cat",Variable("x"),Constant("a"))

      // of course shared root term catT should be same as catS after intergration into shared space
      catT.shared eq catS should be (true)


      // assume we crate a new TEMP function , that cotains already shared terms but is not itself shared
      val catT2 = Predicate("cat2",Variable("x"),Constant("a"))

      // after integation the literals of this function sould point to the already shared terms


      // and the individual literals of the catT.shared should also be equal to the already existing
      // terms a and x
      catT2.args(0) == x should be (true)
      catT2.args(1) == a should be (true)

      catT2.args(0) eq x should be (false)
      catT2.args(1) eq a should be (false)

      catT2.shared.args(0) == x should be (true)
      catT2.shared.args(1) == a should be (true)

      catT2.shared.args(0) eq x should be (true)
      catT2.shared.args(1) eq a should be (true)


    }

    it("shared negation literals should correcly reference previous cached terms") {
      // first assume we have some shared terms In this adress space
      val a = ConstantS("a")
      val x = VariableS("x")
      val catS = PredicateS("cat",x,a)



      val catT = Predicate("cat",Variable("x"),Constant("a"))

      val nCatT = Negation(catT)

      nCatT.shared.asInstanceOf[Negation].filler == catT should be(true)
      nCatT.shared.asInstanceOf[Negation].filler == catS should be(true)
      val filler = nCatT.shared.asInstanceOf[Negation].filler
      filler eq catS should be(true)



      // and the individual literals of the catT.shared should also be equal to the already existing
      // terms a and x
//      catT2.args(0) == x should be (true)
//      catT2.args(1) == a should be (true)
//
//      catT2.args(0) eq x should be (false)
//      catT2.args(1) eq a should be (false)
//
//      catT2.shared.args(0) == x should be (true)
//      catT2.shared.args(1) == a should be (true)
//
//      catT2.shared.args(0) eq x should be (true)
//      catT2.shared.args(1) eq a should be (true)


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