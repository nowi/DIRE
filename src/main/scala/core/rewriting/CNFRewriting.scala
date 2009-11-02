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
    // TODO rewriteClause the sentence into cnf form
    node


  }

}



//trait ImplicationsOut extends FOLNodeRewriting {
//  def rewriteClause(node: FOLNode): FOLNode = {
//    node match {
//    // Eliminate <=>, bi-conditional elimination,
//    // replace (alpha <=> beta) with (~alpha V beta) ^ (alpha V ~beta).
//      case EqualityConnective(left, right) => {
//        val alpha = rewriteClause(left)
//        val beta = rewriteClause(right)
//        val first = OrConnective(Negation(alpha), beta)
//        val second = OrConnective(alpha, Negation(beta))
//        AndConnective(first, second)
//      }
//
//      // Eliminate =>, implication elimination,
//      // replacing (alpha => beta) with (~alpha V beta)
//      case ImplicationConnective(left, right) => {
//        val alpha = rewriteClause(left)
//        val beta = rewriteClause(right)
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
//  def rewriteClause(node: FOLNode): FOLNode = {
//    node match {
//    // CNF requires NOT (~) to appear only in literals, so we 'move ~
//    // inwards' by repeated application of the following equivalences:
//
//      case Negation(filler) => {
//        filler match {
//        // ~(~alpha) equivalent to alpha (double negation elimination)
//          case Negation(filler2) => rewriteClause(filler2)
//
//          // ~(alpha ^ beta) equivalent to (~alpha V ~beta) (De Morgan)
//          case AndConnective(left, right) => {
//            OrConnective(Negation(rewriteClause(left)), Negation(rewriteClause(right)))
//          }
//
//          // ~(alpha V beta) equivalent to (~alpha ^ ~beta) (De Morgan)
//          case OrConnective(left, right) => {
//            AndConnective(Negation(rewriteClause(left)), Negation(rewriteClause(right)))
//
//          }
//
//          // in addition, rules for negated quantifiers:
//
//          // ~FORALL x p becomes EXISTS x ~p
//          case UniversalQuantifer(filler, variables) => {
//            ExistentialQuantifer(Negation(rewriteClause(filler)), variables)
//          }
//
//          // ~EXISTS x p becomes FORALL x ~p
//          case ExistentialQuantifer(filler, variables) => {
//            UniversalQuantifer(Negation(rewriteClause(filler)), variables)
//          }
//
//          case _ => Negation(rewriteClause(filler))
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
//  def rewriteClause(node: FOLNode): FOLNode = {
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


