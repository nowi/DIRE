package core


import containers._

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

  // all clauses that have already been selected for inference
  var workedOff: ClauseStore

  // all candidate clauses to generate inferences
  var usable: ClauseStore

  def prove(clauses: ClauseStore): ProvingResult = {
    workedOff = CNFClauseStore(List())

    // perform input reduction
    usable = taut(sub(clauses))

    while (!usable.isEmpty && !usable.containsEmptyClause) {
      // 5. select a clause
      val given = choose(usable)


      // 5. 6. add to workedoff, remove from usable
      usable = usable -- given
      workedOff = workedOff ++ given

      // 7. all resolution inference conlusions between given and workedoff and all
      // factoring inference conclusions from given are stored in fresh
      var fresh = resolve(given, workedOff) ++ factor(given)

      // 8. - 11.  Perform reductions/forward contractions
      // remove all tautologies and subsumptions from fresh
      fresh = taut(sub(fresh))

      // remove all clauses that are subsumed by a clause in workedoff or usable are deleted
      // from fresh ( forward subsumtion )
      fresh = sub(sub(fresh, workedOff), usable)

      // clasuse remaining in fresh are then used for backward subsumtion
      workedOff = sub(workedOff, fresh)

      // finally add the clauses from fresh to usable , theese are the kept clauses
      usable = sub(usable, fresh) ++ fresh
    }

    if (usable.containsEmptyClause) {
      ProofFound()
    } else if (usable.isEmpty) {
      CompletionFound()
    } else {
      error("Some error occured during prooving, there has been no prooving result")
    }


  }


  def resolve(a: ClauseStore, b: ClauseStore): ClauseStore = {
    CNFClauseStore()

  }

  def choose(clauses: ClauseStore): ClauseStore = {
    CNFClauseStore()
  }

  def factor(clauses: ClauseStore): ClauseStore = {
    CNFClauseStore()

  }

  def taut(clauses: ClauseStore): ClauseStore = {
    CNFClauseStore()
  }

  def sub(a: ClauseStore, b: ClauseStore): ClauseStore = {
    CNFClauseStore()

  }


  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def sub(a: ClauseStore): ClauseStore = {
    CNFClauseStore()
  }


}






case class ProvingResult
case class CompletionFound extends ProvingResult
case class ProofFound extends CompletionFound