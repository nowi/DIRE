package core


import containers._
import net.lag.logging.Logger


/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 15:20:54
 *
 * A first Resolution based prover
 *
 * Mirrors the example prover from Chapter 27 - Combining superposition sorts and splitting
 * from the Automated Theorem Proving Book Volume II
 */

trait Proving {
  val log = Logger.get

  def prove(clauses: ClauseStore): ProvingResult = {
    log.info("Starting theorem proving on clause store %s", clauses)


    // all clauses that have already been selected for inference
    var workedOff: ClauseStorage = CNFClauseStore(List())

    // all candidate clauses to generate inferences
    // perform input reduction
    var usable: ClauseStorage = taut(sub(clauses))

    while (!usable.isEmpty && !usable.containsEmptyClause) {
      log.info("Inner Loop")
      // 5. select a clause
      val given: ClauseStorage = choose(usable)
      log.info("After 5. Given Clause %s", given)

      // 5. 6. add to workedoff, remove from usable
      usable = usable -- given
      workedOff = workedOff ++ given
      log.info("After 5. 6.  usable : %s", usable)
      log.info("After 5. 6.  workedOff : %s", workedOff)

      // 7. all resolution inference conlusions between given and workedoff and all
      // factoring inference conclusions from given are stored in fresh
      var fresh: ClauseStorage = resolve(given, workedOff) ++ factor(given)
      log.info("After 7. fresh Clause %s", fresh)

      // 8. - 11.  Perform reductions/forward contractions
      // remove all tautologies and subsumptions from fresh
      fresh = taut(sub(fresh))
      log.info("After 11. Reduced Fresh Clause %s", fresh)

      // remove all clauses that are subsumed by a clause in workedoff or usable are deleted
      // from fresh ( forward subsumtion )
      fresh = sub(sub(fresh, workedOff), usable)
      log.info("After Forward Subsumption Fresh Clause %s", fresh)


      // clasuse remaining in fresh are then used for backward subsumtion
      workedOff = sub(workedOff, fresh)
      log.info("After Backward Subsumption workedOff Clauses : %s", workedOff)

      // finally add the clauses from fresh to usable , theese are the kept clauses
      usable = sub(usable, fresh) ++ fresh
      log.info("After Addition usable  Clauses are : %s", usable)
    }

    if (usable.containsEmptyClause) {
      log.info("Proof found")
      ProofFound()
    } else if (usable.isEmpty) {
      log.info("Completion found")
      CompletionFound()
    } else {
      log.error("Some error occured during prooving, there has been no prooving result")
      error("Some error occured during prooving, there has been no prooving result")
    }


  }


  def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    CNFClauseStore()

  }

  def choose(clauses: ClauseStorage): ClauseStorage = {
    CNFClauseStore()
  }

  def factor(clauses: ClauseStorage): ClauseStorage = {
    CNFClauseStore()

  }

  def taut(clauses: ClauseStorage): ClauseStorage = {
    CNFClauseStore()
  }

  def sub(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    CNFClauseStore()

  }


  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def sub(a: ClauseStorage): ClauseStorage = {
    CNFClauseStore()
  }


}


class ResolutionProover1 extends Proving {
}




case class ProvingResult
case class CompletionFound extends ProvingResult
case class ProofFound extends CompletionFound