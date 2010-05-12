package core.config

import domain.fol.ast._

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 13:00:09
 */

object WestDomain {
  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")
  val west = Constant("West")
  val nono = Constant("Nono")
  val m1 = Constant("M1")
  val america = Constant("America")
  val sells = (x: FOLNode, y: FOLNode, z: FOLNode) => Predicate("Sells", x, y, z)
  val weapon = (x: FOLNode) => Predicate("Weapon", x)
  val american = (x: FOLNode) => Predicate("American", x)
  val hostile = (x: FOLNode) => Predicate("Hostile", x)
  val missile = (x: FOLNode) => Predicate("Missile", x)
  val owns = (x: FOLNode, y: FOLNode) => Predicate("Owns", x, y)
  val enemy = (x: FOLNode, y: FOLNode) => Predicate("Enemy", x, y)


  val C1 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
    Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
    Predicate("Criminal", x))

  val C2 = StandardClause(
    Negation(Predicate("Missile", x)),
    Negation(Predicate("Owns", nono, x)),
    Predicate("Sells", west, x, nono)
    )

  val C3 = StandardClause(
    Negation(Predicate("Enemy", x, america)),
    Predicate("Hostile", x)
    )


  val C4 = StandardClause(
    Negation(Predicate("Missile", x)),
    Predicate("Weapon", x)
    )

  val C5 = StandardClause(
    Predicate("Owns", nono, m1)
    )
  val C6 = StandardClause(
    Predicate("Missile", m1)
    )
  val C7 = StandardClause(
    Predicate("American", west)
    )

  val C8 = StandardClause(
    Predicate("Enemy", nono, america)
    )


  val initialClauses = List(C1, C2, C3, C4, C5, C6, C7, C8)

  val CriminalWestGoalClause = StandardClause(
    Negation(Predicate("Criminal", west))
    )

  val NotCriminalWestGoalClause = StandardClause(
    Predicate("Criminal", west)
    )

  val AmericanWestGoalClause = StandardClause(
      Predicate("American", west)
    )


}