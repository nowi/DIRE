package core.reduction


import containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.{FOLNode, Clause}
import helpers.Logging
import org.slf4j.LoggerFactory

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 15:19:43
 */

trait Reducing {
  def reduce(a: ClauseStorage): ClauseStorage
}


trait SubsumptionDeletion extends Reducing {
  def deleteSubsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def deleteSubsumptions(a: ClauseStorage): ClauseStorage
}

class SubsumptionDeleter extends SubsumptionDeletion with Logging {
  

  override def reduce(a: ClauseStorage) = deleteSubsumptions(a)

  override def deleteSubsumptions(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Delete Subsumptions {} with {} ...  for now we check if a is contained in b", a, b)
    // remove the intersection
    
    a

  }

  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  override def deleteSubsumptions(a: ClauseStorage): ClauseStorage = {
    log.trace("Subsumption deletion to all of {}", a)
    a
  }


//  private def subsumes(c1 : Clause,c2:Clause) : Boolean = {
//    if(c1 != c2) {
//         // Ensure c1 has less literals total and that
//         // it is a subset of the c2 clauses -- positive and negative counts
//         if(c1.literals.size < c2.literals.size
//                 && c1.positiveLiterals.size <= c2.positiveLiterals.size
//                 && c1.negativeLiterals.size <= c2.negativeLiterals.size){
//
//
//           // Check if c1  literal/arity names are a subset of the
//					 // c2 clause literal/arity names
//           val signatureC1 = c1.literals.map({x:FOLNode => (x.symbolicName,x.arity)})
//           val signatureC2 = c2.literals.map({x:FOLNode => (x.symbolicName,x.arity)})
//
//
//
//
//
//         } else {
//           // is not smaller , in groundcase this means cannot subsume
//
//         }
//    } else {
//      // they are equal
//    }
//  }
//
//  public boolean subsumes(Clause othC) {
//    boolean subsumes = false;
//
//    // Equality is not subsumption
//    if (!(this == othC)) {
//      // Ensure this has less literals total and that
//      // it is a subset of the other clauses positive and negative counts
//      if (this.getNumberLiterals() < othC.getNumberLiterals()
//          && this.getNumberPositiveLiterals() <= othC
//              .getNumberPositiveLiterals()
//          && this.getNumberNegativeLiterals() <= othC
//              .getNumberNegativeLiterals()) {
//
//        Map<String, List<Literal>> thisToTry = collectLikeLiterals(this.literals);
//        Map<String, List<Literal>> othCToTry = collectLikeLiterals(othC.literals);
//        // Ensure all like literals from this clause are a subset
//        // of the other clause.
//        if (othCToTry.keySet().containsAll(thisToTry.keySet())) {
//          boolean isAPossSubset = true;
//          // Ensure that each set of same named literals
//          // from this clause is a subset of the other
//          // clauses same named literals.
//          for (String pk : thisToTry.keySet()) {
//            if (thisToTry.get(pk).size() > othCToTry.get(pk).size()) {
//              isAPossSubset = false;
//              break;
//            }
//          }
//          if (isAPossSubset) {
//            // At this point I know this this Clause's
//            // literal/arity names are a subset of the
//            // other clauses literal/arity names
//            subsumes = checkSubsumes(othC, thisToTry, othCToTry);
//          }
//        }
//      }
//    }
//
//    return subsumes;
//  }
//
//






















}


trait TautologyDeletion extends Reducing {
  def deleteTautologies(clauses: ClauseStorage): ClauseStorage


}

class TautologyDeleter extends TautologyDeletion {
  private lazy val log = LoggerFactory getLogger (this getClass)

  override def deleteTautologies(clauses: ClauseStorage): ClauseStorage = {
    log.trace("Tautology elemination on {} by {}", clauses, this)
    clauses
  }


  override def reduce(a: ClauseStorage) = deleteTautologies(a)
}