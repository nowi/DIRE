package domain.fol.functions


import ast._
import collection.mutable.Stack
import scala.collection.mutable.{Map => MMap}
/**
 * User: nowi
 * Date: 01.04.2010
 * Time: 16:09:37
 */

object FOLAlgorithms extends helpers.Logging {
  def mgu(term1: FOLNode, term2: FOLNode): Option[Substitution] = {
    // short circuit same term
    // first pointer compare , next equals
    if (term1 eq term2) {
      log.ifInfo("Trivial unification detected by jvm equality")
      Some(MMap.empty)
    } else if (term1 == term2) {
      log.ifInfo("Trivial unification detected by OBJECT equality")
      Some(MMap.empty)
    } else {
      (term1, term2) match {
        case (NegativeFOLLiteral(x), PositiveFOLLiteral(y)) => {
          log.ifDebug("Searching for a unfier for terms of different polarity... was this intended ?")
        }

        case (PositiveFOLLiteral(x), NegativeFOLLiteral(y)) => {
          log.ifDebug("Searching for a unfier for terms of different polarity... was this intended ?")
        }

        case _ =>
      }



      var unifies = true;

      var dopop = false

      var s = term1
      var t = term2
      var cS = Context()
      var cT = Context()

      // initialize a mutable stack
      //    val stack = new Stack[(FOLNode, Context)]()
      val stack = new Stack[FOLNode]()



      do {
        if (dopop) {
          dopop = true
          //var popped =
          s = stack.pop
          //cS = popped._2

          //popped = stack.pop
          t = stack.pop
          //cT = popped._2
        }

        // dereference
        s = s.rewrite(cS)
        t = t.rewrite(cT)

        (s, t) match {
          


          case (Predicate(sTop, sTerms), Predicate(tTop, tTerms)) if (sTop == tTop) => {
            // push all terms onto the stack
            for (i <- 0 until sTerms.size) {
              stack.push(sTerms(i))
              stack.push(tTerms(i))
            }

            s = sTerms.head
            t = tTerms.head

            // continue
            dopop = false

          }

          case (Function(sTop, sTerms), Function(tTop, tTerms)) if (sTop == tTop) => {
            // push all terms onto the stack
            for (i <- 0 until sTerms.size) {
              stack.push(sTerms(i))
              stack.push(tTerms(i))
            }

            s = sTerms.head
            t = tTerms.head

            // continue
            dopop = false

          }

          case (s, t) if (s.top == t.top) => {
            // loop
            dopop = true

          }

          case (variable: Variable, _) => {

            if (occurCheck(variable, t, cS, cT)) {
              unifies = false
              stack.clear
            } else {
              cT.bind(variable, t)
              // loop
              dopop = true
            }

          }

          case (_, variable: Variable) => {
            if (occurCheck(variable, s, cT, cS)) {
              unifies = false
              stack.clear
            } else {
              cS.bind(variable, s)
              // loop
              dopop = true
            }

          }

          case _ => {
            unifies = false
            stack.clear
          }

        }


      } while (!stack.isEmpty)

      if (unifies) {
        // merged
        val unifiers = (cT ++ cS)
        // safe guard that there was not a name clash
        assert(unifiers.size == cT.size + cS.size, "There seemes to be a name clash during this unification, have both terms" +
                "been standardized apart ?")
        Some(unifiers)
      } else None

    }


  }


  def occurCheck(v: Variable, t: FOLNode, cS: Context, cT: Context): Boolean = {
    t.flatArgs.contains(v) match {
      case true => {
//        println("OCCURS CHECK FIRED !")
        true
      }

      case false => false
    }

  }


  def matcher(term1: FOLNode, term2: FOLNode): Option[Substitution] = {
//    (term1, term2) match {
//      case (NegativeFOLLiteral(x), PositiveFOLLiteral(y)) => {
//        log.debug("Searching for a matcher for terms of different polarity... was this intended ?")
//      }
//
//      case (PositiveFOLLiteral(x), NegativeFOLLiteral(y)) => {
//        log.debug("Searching for a matcher for terms of different polarity... was this intended ?")
//      }
//
//      case _ =>
//    }


    // shourt cut the trivial matcher
    if (term1 eq term2) {
      log.ifDebug("Matched terms %s and %s by jvm identity",term1,term2)
      Some(Substitution())
    } else if (term1 == term2) {
      log.ifDebug("Matched terms %s and %s by structural identity",term1,term2)
      Some(Substitution())
    }
    else {
      // the order is important !
      var s = term1
      var t = term2


      var matches = true

      var context: Context = null

      // initialize a mutable stack
      val stack = new Stack[FOLNode]()

      var dopop = false

      if (!(s eq t)) { // compare if not same term

        do {
          if (dopop) {
            dopop = true
            t = stack.pop
            s = stack.pop
          }

          (s, t) match {

            case (Predicate(sTop, sTerms), Predicate(tTop, tTerms)) if (sTop == tTop) => {
              // push all terms onto the stack
              for (i <- 0 until sTerms.size) {
                stack.push(sTerms(i))
                stack.push(tTerms(i))
              }

              s = sTerms.head
              t = tTerms.head

              // continue
              dopop = false

            }

            case (Function(sTop, sTerms), Function(tTop, tTerms)) if (sTop == tTop) => {
              // push all terms onto the stack
              for (i <- 0 until sTerms.size) {
                stack.push(sTerms(i))
                stack.push(tTerms(i))
              }

              s = sTerms.head
              t = tTerms.head

              // continue
              dopop = false

            }

            case (s, t) if (s.top == t.top) => {
              // loop
              dopop = true

            }

            case (variable: Variable, _) => {
             if(context==null)context = Context()

              context.binding(variable) match {
                case None => {
                  context.bind(variable, t)
                }

                case Some(binding) if (binding != t) => {
                  matches = false
                  stack.clear
                }


                case _ => {
                  // skip

                }

              }

              // loop
              dopop = true
            }


            case _ => {
              matches = false
              stack.clear
              // loop
              dopop = true
            }

          }


        } while (!stack.isEmpty)
      }


      if (matches) {
        Some(context)
      } else {
        None
      }

    }


  }

  /**Rewrite based on mapping theta
   * @param theta - the mapping
   * @returns rewritten folnode
   */
  def rewrite(node: FOLNode, s: Substitution): FOLNode = {
    // check all possible fol types
    // define the replacement function
    val f = (node: FOLNode, theta: Map[Variable, FOLNode]) => {
      node match {
        case x: Variable => {
          // check if there is a substitution
          theta.getOrElse(x, node)
        }
        case _ => node
      }
    }

    // apply this function partially , theta is fixed
    node.map(f(_: FOLNode, s))


  }

}