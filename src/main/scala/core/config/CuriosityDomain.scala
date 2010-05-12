package core.config

import domain.fol.ast._

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 12:58:51
 */

object CuriosityDomain {
  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")

  val tuna = Constant("Tuna")
  val jack = Constant("Jack")
  val curiosity = Constant("Curiosity")
  val loves = (x: FOLNode, y: FOLNode) => Predicate("Loves", x, y)
  val kills = (x: FOLNode, y: FOLNode) => Predicate("Kills", x, y)
  val cat = (x: FOLNode) => Predicate("Cat", x)
  val animal = (x: FOLNode) => Predicate("Animal", x)
  val f = (x: FOLNode) => Predicate("F", x)
  val g = (x: FOLNode) => Predicate("G", x)


  val A1 = StandardClause(animal(f(x)), loves(g(x), x))
  val A2 = StandardClause(Negation(loves(x, f(x))), loves(g(x), x))
  val B = StandardClause(Negation(animal(y)), Negation(kills(x, y)), Negation(loves(z, x)))
  val C = StandardClause(Negation(animal(x)), loves(jack, x))
  val D = StandardClause(kills(jack, tuna), kills(curiosity, tuna))
  val E = StandardClause(cat(tuna))
  val F = StandardClause(Negation(cat(x)), animal(x))

  val initialClauses = List(A1,A2,B,C,D,E,F)

  val curiosityKilledTunaGoalClause = StandardClause(Negation(kills(curiosity, tuna)))
  val curiosityNotKilledTunaGoalClause = StandardClause(kills(curiosity, tuna))
}