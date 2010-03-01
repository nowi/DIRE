package core

import domain.fol.ast._

import helpers.Logging
import rewriting.{Substitution}

/**
 * User: nowi
 * Date: 09.10.2009
 * Time: 14:59:57
 */

trait Unify {

  /**
   * <code>
   * function UNIFY(x, y, theta) returns a substitution to make x and y identical
   *   inputs: x, a variable, constant, list, or compound
   *           y, a variable, constant, list, or compound
   *
   * @return a Map<Variable, Term> representing the substitution (i.e. a set
   *         of variable/term pairs, see pg. 254 for a description) emmpty if failed to unify
   */
  def unify(x: FOLNode, y: FOLNode): Option[Map[Variable, FOLNode]]

  def unifyReverseRenaming(x: FOLNode, y: FOLNode): (Option[Map[Variable, FOLNode]]) 

  def unify(c1: FOLClause, c2: FOLClause): Option[Map[Variable, FOLNode]]

  def unifyReverseRenaming(c1: FOLClause, c2: FOLClause) : Option[Map[Variable, FOLNode]]
  
  def unify(c1: FOLClause, c2: FOLClause, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]]

  def unify(x: FOLNode, y: FOLNode, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]]

  def firstUnifier(c1: FOLClause): Option[Map[Variable, FOLNode]]
}

// standard unification implementation
class Unificator(env: {val substitutor: Substitution; val standardizer: Standardizing}) extends Unify with Logging {
  val substitutor = env.substitutor
  val standardizer = env.standardizer

  override def unify(x: FOLNode, y: FOLNode): Option[Map[Variable, FOLNode]] = {
    // standardize apart the terms           rd
    if (x == y) {
      log.trace("%s trivially unified equal nodes : %s , %s", this, x, y)
      Some(Map[Variable, FOLNode]())
    }
    val (xr, yr,renamings) = standardizer.standardizeApart(x, y)
    unify(xr, yr, Some(Map[Variable, FOLNode]()))

  }



  def unifyReverseRenaming(c1: FOLClause, c2: FOLClause) : Option[Map[Variable, FOLNode]] = {
val (cs1, cs2,renamings) = standardizer.standardizeApart(c1, c2)



    // we need a global theta for this
    var globalTheta: Map[Variable, FOLNode] = Map[Variable, FOLNode]()

    // unify all literals of these clauess
    val thetas = for (l1 <- cs1.literals;
                      l2 <- cs2.literals;
                      if (l1 != l2)
    ) {
        unify(l1, l2, Some(globalTheta)) match {
          case Some(x) => {
            // there was a unification , set this as our new theta
            log.trace("Success unifying literals : %s with %s ...", l1, l2)
            log.trace("New global theta is : %s ", x)
            globalTheta = x
          }
          case None => {
            // ignore
            log.trace("Could not unify literal : %s with %s ... ignoring", l1, l2)
          }

        }
      }


    if(globalTheta.size > 0) {
      val mgu = Some(globalTheta)

      if(renamings.size > 0) {
        val renamedMgu :Option[Map[Variable, FOLNode]] = mgu match {
          case Some(unifier : Map[Variable, FOLNode]) => {
            // apply reverse renaming
            Some((for((key,value) <- unifier)
            yield (key,substitutor.substitute(Some(renamings),value))
                    ).foldLeft(Map[Variable, FOLNode]())(_ + _))
          }

        }
        renamedMgu

      } else {
        mgu
      }

    } else {
      None
    }

  }


  override def unifyReverseRenaming(x: FOLNode, y: FOLNode): (Option[Map[Variable, FOLNode]]) = {
    // standardize apart the terms           rd
    if (x == y) {
      log.trace("%s trivially unified equal nodes : %s , %s", this, x, y)
      Some(Map[Variable, FOLNode]())
    }
    val (xr, yr,renamings) = standardizer.standardizeApart(x, y)
    val mgu = unify(xr, yr, Some(Map[Variable, FOLNode]()))

    if(renamings.size > 0) {
      val renamedMgu :Option[Map[Variable, FOLNode]] = mgu match {
        case Some(unifier : Map[Variable, FOLNode]) => {
          // apply reverse renaming
          Some((for((key,value) <- unifier)
          yield (key,substitutor.substitute(Some(renamings),value))
                  ).foldLeft(Map[Variable, FOLNode]())(_ + _))
        }
        case None => {
          None
        }     
      }
      renamedMgu

    } else {
      mgu
    }

  }


  def unify(c1: FOLClause, c2: FOLClause, t: Option[Map[Variable, FOLNode]]) = {

    t match {
      case Some(theta) => {
        // first standardize both clauses
        val (cs1, cs2,renamings) = standardizer.standardizeApart(c1, c2)
        // we need a global theta for this
        var globalTheta: Map[Variable, FOLNode] = theta

        // unify all literals of these clauess
        val thetas = for (l1 <- cs1.literals;
                          l2 <- cs2.literals;
                          if (l1 != l2)
        ) {
            unify(l1, l2, Some(globalTheta)) match {
              case Some(x) => {
                // there was a unification , set this as our new theta
                log.trace("Success unifying literals : %s with %s ...", l1, l2)
                log.trace("New global theta is : %s ", x)
                globalTheta = x
              }
              case None => {
                // ignore
                log.trace("Could not unify literal : %s with %s ... ignoring", l1, l2)
              }

            }
          }


        log.trace("Final global theta : %s", globalTheta);
        Some(globalTheta)
      }
      case _ => None

    }

  }


  // this methods immediately returns the first unifier it can find
  def firstUnifier(c1: FOLClause): Option[Map[Variable, FOLNode]] = {

    // we need a global theta for this

    // unify all literals of these clauess
    val thetas: List[Option[Map[Variable, FOLNode]]] = for (l1 <- c1.literals.toList;
                                                            l2 <- c1.literals.toList;
                                                            if (l1 != l2)
    ) yield unify(l1, l2, Some(Map[Variable, FOLNode]())) match {
        case Some(x) => {
          // there was a unification , set this as our new theta
          log.trace("Success unifying literals : %s with %s ...", l1, l2)
          Some(x)

        }
        case None => {
          // ignore
          log.trace("Could not unify literal : %s with %s ... ignoring", l1, l2)
          None
        }

      }




    if (!thetas.isEmpty) {
      log.trace("Found first Unifier : %s", thetas.head);
      thetas.head
    }
    else None

  }

  override def unify(c1: FOLClause, c2: FOLClause): Option[Map[Variable, FOLNode]] = {
    // first standardize both clauses
    val (cs1, cs2,renamings) = standardizer.standardizeApart(c1, c2)



    // we need a global theta for this
    var globalTheta: Map[Variable, FOLNode] = Map[Variable, FOLNode]()

    // unify all literals of these clauess
    val thetas = for (l1 <- cs1.literals;
                      l2 <- cs2.literals;
                      if (l1 != l2)
    ) {
        unify(l1, l2, Some(globalTheta)) match {
          case Some(x) => {
            // there was a unification , set this as our new theta
            log.trace("Success unifying literals : %s with %s ...", l1, l2)
            log.trace("New global theta is : %s ", x)
            globalTheta = x
          }
          case None => {
            // ignore
            log.trace("Could not unify literal : %s with %s ... ignoring", l1, l2)
          }

        }
      }


    log.trace("Final global theta : %s", globalTheta);
    Some(globalTheta)

    //    globalTheta match {
    //      case Some(x) => {
    //        log.trace("Final global theta : %s",globalTheta);
    //        Some(x)
    //      }
    //      case None => {
    //        log.warning("No globalTheta could be constructed")
    //        None
    //      }
    //    }
    //    Some(thetas.reduceLeft(_ ++ _))

  }


  def unify(x: FOLNode, y: FOLNode, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    if (x == y) {
      theta
    } else {
      // if theta = failure then return failure
      theta match {
        case Some(t) => {
          (x, y) match {
          // TODO clean this up , we have a problem with negations
            case (Negation(x), Negation(y)) => unify(x, y, theta)
            case (Negation(x), y: FOLNode) => unify(x, y, theta)
            case (x: FOLNode, Negation(y)) => unify(x, y, theta)
            case (x: Variable, y: FOLNode) => unifyVar(x, y, theta)
            case (x: FOLNode, y: Variable) => unifyVar(y, x, theta)
            case (x: Constant, y: Constant) => None
            case (x: FOLNode, y: FOLNode) => {
              //IF COMPOUND?(x) and COMPOUND?(y) then
              //UNIFY(ARGS[x], ARGS[y], UNIFY(OP[x], OP[y], theta))
              unify(x.args, y.args, unify(x.symbolicName, y.symbolicName, theta));
            }
          }


        }
        case None => None

      }

    }


  }


  private def unify(xs: List[FOLNode], ys: List[FOLNode], theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    if (xs.size != ys.size) None
    theta match {
      case Some(t) => {
        (xs, ys) match {
          case (List(), List()) => theta
          case (x :: List(), y :: List()) => unify(x, y, theta)
          case (x :: xsss, y :: ysss) => unify(xsss, ysss, unify(x.asInstanceOf[FOLNode], y.asInstanceOf[FOLNode], theta))
        }

      }
      case None => None
    }

  }

  private def unify(x: String, y: String, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    theta match {
      case Some(t) => {
        if (x == y) theta
        else None
      }
      case None => None

    }

  }

  private def unifyVar(v: Variable, node: FOLNode, theta: Option[Map[Variable, FOLNode]]): Option[Map[Variable, FOLNode]] = {
    theta match {
      case Some(t) => {
        t.get(v) match {
          case Some(value) => unify(value, node, theta)
          // TODO something not right here , investigate
          //    else if (theta.keySet().contains(node)) unify(v, theta(node), theta)
          case None => {
            if (doesVarOccur(theta, v, node)) None
            else {
              cascadeSubstitution(theta, v, node);
            }
          }
        }
      }
      case None => None
    }

  }


  /**
   * Cascading substitutions

  Sometimes you get a substitution of the form σ =                                    { z ← x, x ← a.
  Suppose you were to apply this substitution to p(z,x). The correct result is p(a,a).
  The reason is that you need to "cascade" the substitutions; if z takes the value x,
  you need to make sure that you haven't constrained x to be some other value.
  It would be incorrect to write p(x,a). This has particularly important consequences anytime
  you are trying to unify two expressions.
   */
  private def cascadeSubstitution(theta: Option[Map[Variable, FOLNode]], v: Variable, term: FOLNode): Option[Map[Variable, FOLNode]] = {
    // add the new subsitution
    theta match {
      case Some(t) => {

        val theta2: Map[Variable, FOLNode] = t + (v -> term)
        log.trace("Creating substitution %s -- > %s .., theta = %s",v, term, theta2)
        val theta3 = (for (key <- theta2.keySet) yield Map(key -> substitutor.substitute(Some(theta2),
          theta2.get(key) match {
            case Some(value) => value
          })))

        val theta4 = theta3.reduceLeft((map1, map2) => map1 ++ map2)
        log.trace("Cascading substitution %s -- > %s .., theta = %s", v, term, theta4)
        Some(theta4)


      }
      case None => None
    }

  }


  private def doesVarOccur(theta: Option[Map[Variable, FOLNode]], v: Variable, term: FOLNode): Boolean = {
    term match {
      case x: Function => x.vars contains v
      case _ => false
    }
  }


}


