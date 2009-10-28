package core.rewriting


import domain.fol.ast._

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 17:30:12
 */

trait FOLNodeRewriting {
  def rewrite(node: FOLNode): FOLNode

}


trait CNFRewriting extends FOLNodeRewriting {
  def rewrite(node: FOLNode): FOLNode = {
    // TODO rewrite the sentence into cnf form
    node


  }

}



//trait ImplicationsOut extends FOLNodeRewriting {
//  def rewrite(node: FOLNode): FOLNode = {
//    node match {
//    // Eliminate <=>, bi-conditional elimination,
//    // replace (alpha <=> beta) with (~alpha V beta) ^ (alpha V ~beta).
//      case EqualityConnective(left, right) => {
//        val alpha = rewrite(left)
//        val beta = rewrite(right)
//        val first = OrConnective(Negation(alpha), beta)
//        val second = OrConnective(alpha, Negation(beta))
//        AndConnective(first, second)
//      }
//
//      // Eliminate =>, implication elimination,
//      // replacing (alpha => beta) with (~alpha V beta)
//      case ImplicationConnective(left, right) => {
//        val alpha = rewrite(left)
//        val beta = rewrite(right)
//        OrConnective(Negation(alpha), beta)
//      }
//
//      // default , return node as is
//      case _ => node
//    }
//
//  }
//
//}
//
//
//trait NegationsIn extends FOLNodeRewriting {
//  def rewrite(node: FOLNode): FOLNode = {
//    node match {
//    // CNF requires NOT (~) to appear only in literals, so we 'move ~
//    // inwards' by repeated application of the following equivalences:
//
//      case Negation(filler) => {
//        filler match {
//        // ~(~alpha) equivalent to alpha (double negation elimination)
//          case Negation(filler2) => rewrite(filler2)
//
//          // ~(alpha ^ beta) equivalent to (~alpha V ~beta) (De Morgan)
//          case AndConnective(left, right) => {
//            OrConnective(Negation(rewrite(left)), Negation(rewrite(right)))
//          }
//
//          // ~(alpha V beta) equivalent to (~alpha ^ ~beta) (De Morgan)
//          case OrConnective(left, right) => {
//            AndConnective(Negation(rewrite(left)), Negation(rewrite(right)))
//
//          }
//
//          // in addition, rules for negated quantifiers:
//
//          // ~FORALL x p becomes EXISTS x ~p
//          case UniversalQuantifer(filler, variables) => {
//            ExistentialQuantifer(Negation(rewrite(filler)), variables)
//          }
//
//          // ~EXISTS x p becomes FORALL x ~p
//          case ExistentialQuantifer(filler, variables) => {
//            UniversalQuantifer(Negation(rewrite(filler)), variables)
//          }
//
//          case _ => Negation(rewrite(filler))
//
//
//        }
//
//
//      }
//
//
//    }
//
//  }
//
//}
//trait RemoveQuantifiers extends FOLNodeRewriting {
//  def rewrite(node: FOLNode): FOLNode = {
//    node match {
//    // Skolemize: Skolemization is the process of removing existential
//    // quantifiers by elimination. This is done by introducing Skolem
//    // functions. The general rule is that the arguments of the Skolem
//    // function are all the universally quantified variables in whose
//    // scope the existential quantifier appears.
//      case ExistentialQuantifer(filler, variables) => {
//        // substition list
//        val theta = Map[Variable,FOLNode]()
//        val skolemSubst = variables.map({ v:Variable => Map(v,Function(skolemName,) })
//
//      }
//
//    }
//
//  }
//
//}


