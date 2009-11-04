package core


import collection.immutable.{Map => ImmutableMap}
import collection.mutable.{Map => MutableMap}
import domain.fol.ast.{Constant, Variable, Clause, FOLNode}
import rewriting.Substitution

/**
 * User: nowi
 * Date: 04.11.2009
 * Time: 13:20:08
 */

class RecursiveDescentUnificator(env: {val substitutor: Substitution; val standardizer: Standardizing}) extends Unify {
  val substitutor = env.substitutor
  val standardizer = env.standardizer

  val log = net.lag.logging.Logger.get
  var substitutions: MutableMap[Variable, FOLNode] = MutableMap[Variable, FOLNode]()

  def unify(x: FOLNode, y: FOLNode): Option[ImmutableMap[Variable, FOLNode]] = {
    // init the global subs map
    substitutions = MutableMap[Variable, FOLNode]()

    // standardize apart both nodes
    val (xr, yr) = standardizer.standardizeApart(x, y)

    // unify recursivly
    unifyRec(xr, yr)

    // return the substittions
    if (substitutions.isEmpty) {
      None
    } else {
      Some(ImmutableMap[Variable, FOLNode]() ++ substitutions)
    }


  }

  private def unifyRec(n1: FOLNode, n2: FOLNode) {
    log.info("Trying to unify %s with %s", n1, n2)
    var s = n1
    var t = n2
    if (s.isInstanceOf[Constant] && t.isInstanceOf[Constant]) {
      return
    }

    if (s.isInstanceOf[Variable]) {
      // apply subs on variable x
      s = applySubstitution(s.asInstanceOf[Variable])

    }
    if (t.isInstanceOf[Variable]) {
      // apply subs on variable x
      t = applySubstitution(t.asInstanceOf[Variable])

    }

    if (s.isInstanceOf[Variable] && s == t) {
      // do nothing
    } else if (s.arity > 0 && t.arity > 0 && s.arity == t.arity) {
      if (s.symbolicName == t.symbolicName) {
        // unifyrec each term
        for (
          (ti, si) <- s.args zip t.args
        ) unifyRec(ti, si)

      } else {
        // exit with failure , symbolic clash
        return
      }
    } else if (s.arity != t.arity) {
      log.warning("Cannot unify %s with %s -- they have different arity", s, t)
      return
    } else if (!s.isInstanceOf[Variable]) {
      unifyRec(t, s)
    } else if (t.args contains s) {
      // exit with failure -- occurs check
      return
    } else {
      createSubstition(s.asInstanceOf[Variable], t)
    }


  }

  override def unify(c1: Clause, c2: Clause): Option[Map[Variable, FOLNode]] = {
    // first standardize both clauses
    val (cs1, cs2) = standardizer.standardizeApart(c1, c2)


    // unify all literals of these clauess
    val thetas = for (l1 <- cs1.literals;
                      l2 <- cs2.literals;
                      if (l1 != l2)
    )
    yield unify(l1, l2) match {
        case Some(theta) => theta
        case None => ImmutableMap[Variable, FOLNode]()
      }
    Some(thetas.reduceLeft(_ ++ _))

  }

  private def applySubstitution(v: Variable): FOLNode = {
    // return substition for v or v itself if there is no substition
    val subs: FOLNode = substitutions.getOrElse(v, v)
    if (subs != v) log.info("Substituted Variable %s with %s", v, subs)
    subs
  }

  private def createSubstition(v: Variable, subs: FOLNode) {
    log.info("Saving Substitution %s -- > %s", v, subs)
    substitutions.put(v, subs)
  }


}