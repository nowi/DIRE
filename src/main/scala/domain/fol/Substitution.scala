package domain.fol


import ast.{IndicatorVariable, Term, Variable, FOLNode}
import collection.immutable.EmptySet
import collection.{SortedMap, MapProxy}
import scala.collection.mutable.{Map => MMap}
import functions.FOLAlgorithms
import helpers.HelperFunctions._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import FOLAlgorithms._

/**
 * User: nowi
 * Date: 01.04.2010
 * Time: 13:24:29
 */

case class Substitution(override val self: MMap[Variable, FOLNode]) extends MapProxy[Variable, FOLNode] {
  lazy val domain: List[Variable] = keys.toList

  lazy val codomain: List[FOLNode] = values.toList

  lazy val image: List[Variable] = {
    codomain.map(_.flatArgs.filter({
      node: FOLNode => node match {
        case Variable(x) => true
        case _ => false
      }
    })).flatten(itr => itr).asInstanceOf[List[Variable]]
  }


  def restrict(u: List[Variable]): Substitution = {
    // restrict the substituion that agrees with u on the this substitution
    // filter out all mappings that are not part of u
    this.filter({case (variable, term) => u.contains(variable)})

  }


  override def apply(a: Variable) = {
    throw new IllegalStateException("WARNING .. didnt you really mean rewirte the term according to this substitution")
  }

  def binding(a: Variable) = super.apply(a)


  lazy val normalize: Substitution = {
    // variables have lexicographic order based on variable name
    val lt = (v1: Variable, v2: Variable) => (v1.name < v2.name)
    // get the unique variables in the substitution and sort them
    // crate the first occurrence map
    val sortedUniqueVars = keys.toList.sort(lt)
    val tempSub: MMap[Variable, FOLNode] = MMap()


    // get the unique variables for the terms in the order of sortedUnique keys, and build up
    // substitution map
    for (currentVar <- sortedUniqueVars) {

      // we need the unique variables in this term , that are not already mapped to indicatorVariables in s
      val uniqueVarsTerm = sortedUniqueVars.map(binding(_).flatArgs.filter(_ match {
        case variable: Variable if (!tempSub.contains(variable)) => true
        case _ => false
      })).flatten(itr => itr).asInstanceOf[List[Variable]].foldLeft(Set[Variable]())(_ + _)

      // for each of the unique vars for the current term , add a new mapping Var --> indicator var
      val newVariableMappings = uniqueVarsTerm.toList zip IndicatorVariable.rangeTo(uniqueVarsTerm.size)

      // add the new mappings into s
      tempSub ++= newVariableMappings.asInstanceOf[Iterable[Tuple2[Variable, FOLNode]]]

    }

    // now each term in this substitution with the substituon build up in tempSub
    val transformed = this.map({case (variable, term) => (variable -> term.rewrite(tempSub))})
    transformed
  }

  // add a new single mapping into substituion , must not have variable contained in domain(this)
  def +(substitution: Substitution): Substitution = {
    // first make sure that its a singleton or empty
    if (substitution.isEmpty)
      this
    else {
      require(substitution.size == 1, "Cannot add Substitions that contain more then one element use ++")
      if(domain.contains(substitution.toList.head._1)) {
        require(!domain.contains(substitution.toList.head._1), "must not have variable contained in domain(this)")
      }
      Substitution(self + substitution.toList.head.asInstanceOf[Tuple2[Variable, FOLNode]])
    }
  }

  def compose(r: Substitution): Substitution = {
    val s = this
    // the images of both substitions might be affected by the substitions
    // first apply the substititons to the images
    val sMapd = (s map ({case (xi, si) => (xi -> si.rewrite(r))}))
    val rMapd = (r.filter({case (yi, ti) => (r.domain -- s.domain).contains(yi)}))
    (sMapd ++ rMapd)
  }

  // join
  def join(r: Substitution): Substitution = {
    val s = this
    // the images of both substitions might be affected by the substitions
    // first apply the substititons to the images
    val sMapd = (s map ({case (xi, si) => (xi -> si.rewrite(r))}))
    val rMapd = (r.filter({case (yi, ti) => (r.domain -- s.image).contains(yi)}))
    (sMapd ++ rMapd)

  }
}


// disjoiint union extractor for substititutions
object DU {
  def unapply(in: Substitution): Option[Tuple2[Substitution, Substitution]] = {
    in.toList match {
      case head :: tail => Some(head, tail)
      case _ => None
    }
  }
}

// singleton substitution extractor
object Singleton {
  def unapply(in: Substitution): Option[Tuple2[Variable, FOLNode]] = {
    if (in.size == 1)
      Some(in.toList.head)
    else
      None
  }
}

object NonTrivialSubstitution {
  def unapply(in: Substitution): Option[Substitution] = {
    if (!in.isEmpty)
      Some(in)
    else
      None
  }
}


object Substitution {
  def apply(): Substitution = MMap()

  def apply(immutable : Map[Variable,FOLNode]) : Substitution = {
    Substitution(MMap() ++ immutable)
  }

  //  def apply(iter : Seq[Tuple2[Variable,FOLNode]]): Substitution = Substitution(Map[Variable,FOLNode](iter :_*))

  // sequence extractor


  /**
   * The test function 'generalization' checks for every assignment of the substitution s1
   * if under the current variable bindings denoted by s2 , a Simultaneous matcher m from all
   * terms of the codomain to the correspoding bindings of the domain variables exists
   */
  def generalizations(r: Substitution, p: Substitution): Option[Substitution] = {
    if (r.isEmpty)
      Some(r)
    else {
      if (r != p) {
        val ms = r.domain.map({
          xi: Variable => {
            val gen = xi.rewrite(r).rewrite(p)
            val inst = xi.rewrite(p)
            matcher(gen, inst)
          }
        })

        // convert
        convertIterableOfOptions(ms) match {
          case Some(NonTrivialSubstitution(matcher) :: Nil) => {
            // only one element , looks right
            Some(matcher)
          }
          case Some(emptyMatcher :: Nil) => {
            // only a empty matcher 
            // return trivial
            Some(Substitution())
          }

          case _ => {
            //error("Error in finding simulatanous matcher")
            None
          }
        }

      } else {
        // return trivial
        Some(Substitution())
      }

    }


  }

  def unifiers(r: Substitution, p: Substitution): Option[Substitution] = {
    if (r.isEmpty)
      Some(r)
    else {
      if (r != p) {
        val ms = r.domain.map({
          xi: Variable => {
            val gen = xi.rewrite(r).rewrite(p)
            val inst = xi.rewrite(p)
            mgu(gen, inst)
          }
        })

        // TODO , here check If MOST GENERAL !!!!!!!!


        // convert
        convertIterableOfOptions(ms) match {
          case Some(NonTrivialSubstitution(unifier) :: Nil) => {
            // only one element , looks right
            Some(unifier)
          }
          case Some(emptyUnifier :: Nil) => {
            // only a empty matcher
            // return trivial
            Some(Substitution())
          }

          case _ => {
            //error("Error in finding simulatanous matcher")
            None
          }
        }

      } else {
        // return trivial
        Some(Substitution())
      }

    }


  }


  def instances(s1: Substitution, s2: Substitution): Option[Substitution] = {
    if (s1.isEmpty)
      Some(s1)
    else unifiers(s1, s2) match {
      case Some(unifier) => {
        // the intersection of mgu and (domain(unfier) ++ indicator variables, has to be empty
        // in other words , the unifers domain cannot contain a indicator variable
        unifier.domain.forall(!_.isInstanceOf[IndicatorVariable]) match {
          case true => Some(unifier)
          case false => None
        }

      }

      case None => None // there has been no unfier to begin with
    }
  }

  def variants(r: Substitution, s: Substitution): Option[Substitution] = {
    //println("Calculating variants for %s and %s" format (r, s))
    if (r.isEmpty)
      Some(r)
    else {
      generalizations(r, s) match {
        case Some(variantMatcher) => {
          // the intersection of matcher and (domain(matcher) ++ indicator variables, has to be empty
          // in other words , the matcher domain cannot contain a indicator variable
          variantMatcher.domain.forall(!_.isInstanceOf[IndicatorVariable]) match {
            case true => {
              //println("Found variant matcher %s , for %s and %s" format (variantMatcher, r, s))
              Some(variantMatcher)
            }
            case false => None
          }

        }

        case None => None // there has been no unfier to begin with
      }

    }
  }


  implicit def iterableToSubs(iter: Iterable[Tuple2[Variable, FOLNode]]): Substitution = {
    Substitution(MMap() ++ iter )
  }

  implicit def tupleToSubs(tuple: Tuple2[Variable, FOLNode]): Substitution = {
    Substitution(MMap(tuple))

  }




  implicit def subsToTuples(sub: Substitution): Iterable[Tuple2[Variable, FOLNode]] = {
    sub.toList
  }

  implicit def tuplesToSubs(tuples: List[Tuple2[Variable, FOLNode]]): Substitution = {

//    Substitution((MMap[Variable, B]() /: tuples)(_ + _))
    Substitution(MMap[Variable, FOLNode]() ++ tuples)

  }

  implicit def subToMap(sub: Substitution): Map[Variable, FOLNode] = {
    sub.asInstanceOf[Map[Variable, FOLNode]]

  }


}