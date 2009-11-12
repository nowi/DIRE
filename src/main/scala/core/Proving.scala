package core


import containers._
import domain.fol.ast.{FOLClause, Clause}
import net.lag.configgy.Configgy
import org.slf4j.LoggerFactory
import reduction.{Factoring, SubsumptionDeletion, TautologyDeletion}
import resolution.Resolution

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
  def prove(clauses: ClauseStore): ProvingResult
}


class ResolutionProover1(env: {val tautologyDeleter: TautologyDeletion; val subsumptionDeleter: SubsumptionDeletion; val factorizer: Factoring; val resolver: Resolution}) extends Proving {
  Configgy.configure("/Users/nowi/workspace/DIRE/DIRE/config.conf")
  //  val log = LoggerFactory getLogger (this getClass)


  val log = LoggerFactory getLogger (this getClass)

  val rnd = new Random(System.currentTimeMillis)


  val resolver = env.resolver
  val tautologyDeleter = env.tautologyDeleter
  val subsumptionDeleter = env.subsumptionDeleter
  val factorizer = env.factorizer

  override def prove(clauses: ClauseStore): ProvingResult = {
    log.info("Starting theorem proving on clause store {}", clauses)


    // all clauses that have already been selected for inference
    var workedOff: ClauseStorage = CNFClauseStore(Set[FOLClause]())

    // all candidate clauses to generate inferences
    // perform input reduction
    //    var usable: ClauseStorage = taut(sub(clauses))
    var usable: ClauseStorage = clauses

    var iteration = 1;
    while (!usable.isEmpty && !usable.containsEmptyClause) {
      log.trace("Inner Loop")
      log.trace("Inner Loop")
      // 5. select a clause
      val given: ClauseStorage = choose(usable)
      log.info("After 5. Given Clause {}", given)

      // 5. 6. add to workedoff, remove from usable       fa
      usable = usable -- given
      workedOff = workedOff ++ given
      log.info("After 5. 6.  usable : {}", usable)
      log.info("After 5. 6.  workedOff : {}", workedOff)

      // 7. all resolution inference conlusions between given and workedoff and all
      // factoring inference conclusions from given are stored in fresh
      var fresh: ClauseStorage = resolve(given, workedOff) // ++ factor(given)
      log.info("After 7. fresh Clause {}", fresh)

      // 8. - 11.  Perform reductions/forward contractions
      // remove all tautologies and subsumptions from fresh
      fresh = taut(sub(fresh))
      log.trace("After 11. Reduced Fresh Clause {}", fresh)

      // remove all clauses that are subsumed by a clause in workedoff or usable are deleted
      // from fresh ( forward subsumtion )
      fresh = sub(sub(fresh, workedOff), usable)
      log.trace("After Forward Subsumption Fresh Clause {}", fresh)


      // clasuse remaining in fresh are then used for backward subsumtion
      workedOff = sub(workedOff, fresh)
      log.trace("After Backward Subsumption workedOff Clauses : {}", workedOff)

      // finally add the clauses from fresh to usable , theese are the kept clauses
      log.info("Adding to usable FRESH : {}", fresh)
      usable = sub(usable, fresh) ++ fresh
      log.info("Usable Size after iteration {} : {}", iteration, usable.clauses.size)
      log.info("Workedof Size after iteration {} : {}", iteration, workedOff.clauses.size)

      log.trace("After Addition usable  Clauses are : {}", usable)
      iteration += 1
    }

    if (usable.containsEmptyClause) {
      log.info("Proof found")
      log.info("FINAL worked Off was {}", workedOff)
      ProofFound()
    } else if (usable.isEmpty) {
      log.info("Completion found")
      log.info("FINAL worked Off was {}", workedOff)
      CompletionFound()
    } else {
      log.error("Some error occured during prooving, there has been no prooving result")
      error("Some error occured during prooving, there has been no prooving result")
    }


  }


  def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {

    val resolved = resolver.resolve(a, b)
    log.info("Resolved : {}", resolved)
    resolved

  }

  def choose(clauses: ClauseStorage): ClauseStorage = {

    val choosen = clauses.clauses.toList(rnd.nextInt(clauses.clauses.toList.size))
    log.trace("Naively choosing clause : {} from clauses store : {}", choosen, clauses)
    CNFClauseStore(choosen)
  }

  def factor(clauses: ClauseStorage): ClauseStorage = {
    factorizer.factorize(clauses)
  }

  def taut(clauses: ClauseStorage): ClauseStorage = {
    tautologyDeleter.deleteTautologies(clauses)
  }

  def sub(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    subsumptionDeleter.deleteSubsumptions(a, b)
  }


  /**
   * Apply subsumption deletion to all of the clauses in the clauseStore
   * and return the resulting clausestore
   */
  def sub(a: ClauseStorage): ClauseStorage = {
    subsumptionDeleter.deleteSubsumptions(a)
  }


}




case class ProvingResult
case class CompletionFound extends ProvingResult
case class ProofFound extends ProvingResult