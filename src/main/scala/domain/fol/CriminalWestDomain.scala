package domain.fol


import ast._

/**
 * User: nowi
 * Date: 20.12.2009
 * Time: 12:18:19
 */

object CriminalWestDomain {
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


  val C1 = StandardClause( // size 4
    Negation(Predicate("American", x)),
    Negation(Predicate("Weapon", y)),
    Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
    Predicate("Criminal", x))

  val C2 = StandardClause( // size 3
    Negation(Predicate("Missile", x)),
    Negation(Predicate("Owns", nono, x)),
    Predicate("Sells", west, x, nono)
    )

  val C3 = StandardClause( // size 2
    Negation(Predicate("Enemy", x, america)),
    Predicate("Hostile", x)
    )


  val C4 = StandardClause( // size 2
    Negation(Predicate("Missile", x)),
    Predicate("Weapon", x)
    )

  val C5 = StandardClause( // size 1
    Predicate("Owns", nono, m1)
    )
  val C6 = StandardClause( // size 1
    Predicate("Missile", m1)
    )
  val C7 = StandardClause( // size 1
    Predicate("American", west)
    )

  val C8 = StandardClause( // size 1
    Predicate("Enemy", nono, america)
    )

  val goalClause = StandardClause( // size 1
    Negation(Predicate("Criminal", west))
    )
}