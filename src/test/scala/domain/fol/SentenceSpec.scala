package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import ast.{AndConnective}
import com.jteigen.scalatest.JUnit4Runner

import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class SentenceSpec extends Spec with ShouldMatchers {
  //  describe("A Constant") {
  //    it("should have a name equal to the passed value") {
  //      val john = Constant("John")
  //      assert(john.name === "John")
  //    }
  //  }
  //
  //
  //  describe("A and connective with 2 constants") {
  //    it("constructer should work") {
  //      val john = Constant("John")
  //      val richard = Constant("Richard")
  //      val johnandrichard = AndConnective(john, richard)
  //
  //      assert(johnandrichard.left == john)
  //      assert(johnandrichard.right == richard)
  //      println(johnandrichard)
  //
  //
  //    }
  //
  //    it("objects should be reused in subexpressions") {
  //      val john = Constant("John")
  //      val richard = Constant("Richard")
  //      val johnandrichard = AndConnective(john, richard)
  //      val johnandrichard2 = AndConnective(john, richard)
  //      val jarandjar2 = AndConnective(johnandrichard, johnandrichard2)
  //
  //      assert(johnandrichard.left eq johnandrichard2.left)
  //      assert(johnandrichard.right eq johnandrichard2.right)
  //      assert(jarandjar2.right.right eq johnandrichard2.right)
  //      assert(jarandjar2.left.left eq johnandrichard2.left)
  //      println("jarandjar2.left.left == %s" format (jarandjar2.left.left))
  //
  //
  //    }
  //
  //    it("implicit conversions should work") {
  //      val john = Constant("John")
  //      val richard = Constant("Richard")
  //      val johnandrichard = AndConnective(john, richard)
  //      val johnandrichard2 = AndConnective(john, richard)
  //      val jarandjar2 = AndConnective(johnandrichard, johnandrichard2)
  //
  //      assert(jarandjar2.left.left.name eq johnandrichard2.left.name)
  //      println("jarandjar2.left.name == %s" format (jarandjar2.left.name))
  //
  //    }
  //  }
}
