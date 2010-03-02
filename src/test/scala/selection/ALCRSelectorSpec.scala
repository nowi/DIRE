import com.jteigen.scalatest.JUnit4Runner

import core.selection.{DALCRSelector, LiteralSelection}
import domain.fol.ast._
import helpers.Logging
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


@RunWith(classOf[JUnit4Runner])
class ALCRSelectorSpec extends Spec with ShouldMatchers with Logging{
  describe("ALCRSelectorSpec") {

    val selector: LiteralSelection = new DALCRSelector

    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
    val west = Constant("West")
    val nono = Constant("Nono")
    val m1 = Constant("M1")
    val america = Constant("America")
    val sellsPred = Predicate("Sells", x, y, z)
    val sellsFunc = Function("sells", x, y, z)

    val weapon = (x: FOLNode) => Predicate("Weapon", x)
    val american = (x: FOLNode) => Predicate("American", x)
    val hostile = (x: FOLNode) => Predicate("Hostile", x)
    val missile = (x: FOLNode) => Predicate("Missile", x)
    val owns = (x: FOLNode, y: FOLNode) => Predicate("Owns", x, y)
    val enemy = (x: FOLNode, y: FOLNode) => Predicate("Enemy", x, y)


    val C1 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
      Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
      Predicate("Criminal", x))


    it("Basic test") {
      val selection = selector.selectedLiterals(StandardClause(sellsPred, sellsFunc, Negation(sellsPred), Negation(sellsFunc)))
      log.warning("The selection for clause %s was %s", C1, selection)
      assert(selection.contains(Negation(sellsPred)))
      assert(selection.contains(Negation(sellsFunc)))
      assert(!selection.contains(sellsPred))
      assert(!selection.contains(sellsFunc))
    }
  }
}