package core


import collection.immutable.{TreeSet, SortedSet}
import collection.mutable.ListBuffer
import containers._
import domain.fol.ast._
import formatting.ClauseFormatting
import helpers.Logging
import net.lag.configgy.Configgy
import ordering.LiteralComparison
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

trait FOLProving {
  def prove(goalClause: FOLClause): ProvingResult

  def satisfy(): ProvingResult
}




class ResolutionProover1(env: {
  val initialClauses: ClauseStorage;
  val recordProofSteps: Boolean;
  val tautologyDeleter: TautologyDeletion;
  val subsumptionDeleter: SubsumptionDeletion;
  val factorizer: Factoring;
  val resolver: Resolution;
  val removeDuplicates: Boolean;
  val usableBackSubsumption: Boolean;
  val forwardSubsumption: Boolean;
  val literalComparator: LiteralComparison;
  val dropSeenClauses: Boolean;
  val useLightesClauseHeuristic: Boolean})
        extends FOLProving
                with Logging with ClauseFormatting {
  Configgy.configure("/Users/nowi/workspace/DIRE/DIRE/config.conf")


  val initialClauses = env.initialClauses

  val rnd = new Random(System.currentTimeMillis)


  val resolver = env.resolver
  val tautologyDeleter = env.tautologyDeleter
  val subsumptionDeleter = env.subsumptionDeleter
  val factorizer = env.factorizer
  val literalComparator = env.literalComparator

  val removeDuplicates = env.removeDuplicates;
  val useLightesClauseHeuristic = env.useLightesClauseHeuristic
  val usableBackSubsumption = env.usableBackSubsumption
  val forwardSubsumption = env.forwardSubsumption
  val dropSeenClauses = env.dropSeenClauses

  val recordProofSteps = env.recordProofSteps


  override def satisfy() = {
    log.info("Configuration is  {}", env)
    log.info("Starting saturation on clause store {}", initialClauses)
    // all candidate clauses to generate inferences
    // perform input reduction
    //var usable: OrderedSet[FOLClause] = taut(sub(clauses))
    saturate(initialClauses)

  }

  override def prove(goalClause: FOLClause) = {
    log.info("Configuration is  {}", env)

    log.info("Starting refutation proof on clause store {}", initialClauses)
    saturate(goalClause :: initialClauses)
  }

  private def saturate(clauses: ClauseStorage): ProvingResult = {
    var usable: UsableClauseStore = new UsableClauseStore(List[FOLClause]())
    var workedOff: ClauseStorage = new WorkedOffClauseStore

    var seenClauses = Set[FOLClause]()

    // all candidate clauses to generate inferences
    // perform input reduction
    //var usable: OrderedSet[FOLClause] = taut(sub(clauses))
    usable = taut(clauses) ::: usable

    var derivedClausesCount: Int = 0
    var keptClausesCount: Int = 0
    var iteration = 1;
    val startTime = System.currentTimeMillis
    while (!usable.isEmpty && !containsEmptyClause(usable)) {
      log.trace("Inner Loop")
      // 5. select a clause
      // sort the list and remove duplicates
      val headClause: FOLClause = usable.head
      usable = usable.tail


      CNFClauseStore(headClause) match {
        case NonEmptyClauseStore(given) => {
          log.debug("Current Given clause is  {}", given)


          // get the maxLiteral of given

          val maxLit = headClause.maxLit(literalComparator)

          log.trace("Maximum literal of given Clause {} is {}", headClause, maxLit)



          if (seenClauses.contains(headClause) && dropSeenClauses) {
            log.warn("Already have seen this clause : {}.. dropping it", headClause)
          } else {

            // 5. 6. add to workedoff, remove from usable       fa
            workedOff = given ::: workedOff
            seenClauses += headClause
            log.debug("After 5. 6.  usable : {}", usable)
            log.debug("After 5. 6.  workedOff : {}", workedOff)
            //Hydrophilic(U) -> Hydrophobicity(U)*.


            // 7. all resolution inference conlusions between given and workedoff and all
            // factoring inference conclusions from given are stored in fresh
            val resolved = resolve(headClause, workedOff) //++ factor(given)



            // we have some derived clauses
            log.debug("We have successfully derived claueses {}", resolved)

            // 8. - 11.  Perform reductions/forward contractions
            // remove all tautologies and subsumptions from fresh


            resolved match {
              case NonEmptyClauseStore(derivedClauses) => {
                derivedClausesCount = derivedClausesCount + derivedClauses.size
                // self subsumption and tautology remoal
                taut(sub(derivedClauses)) match {
                  case NonEmptyClauseStore(newClause) => {
                    // first kill clauses that are already in workedoff or usable
                    removeDups2(newClause, usable, workedOff) match {
                      case NonEmptyClauseStore(uniqueDerived) => {
                        // forward subsupmtion on fresh

                        val keptClauses = forwardSubsumption match {
                          case true => {
                            sub(sub(uniqueDerived, workedOff), usable)
                          }
                          case false => uniqueDerived
                        }

                        log.trace("After Forward Subsumption Fresh Clause {}", keptClauses)


                        // clasuse remaining in fresh are then used for backward subsumtion
                        workedOff = usableBackSubsumption match {
                          case true => sub(workedOff, keptClauses)
                          case false => workedOff
                        }

                        log.trace("After Backward Subsumption workedOff Clauses : {}", workedOff)

                        // backwardsubsumption on usable based on fresh clause
                        //                        if (usableBackSubsumption) {
                        //                          val beforeSize = usable.size
                        //                          usable = sub(usable, keptClauses)
                        //                          val delta = beforeSize - usable.size
                        //                          if (delta > 0) {
                        //                            log.warn("Backwardsubsumption on usable has deleted {} clauses", delta)
                        //                          }
                        //                        }

                        if (recordProofSteps) {
                          log.info(printClauses(keptClauses, resolver))
                        }
                        usable.enqueue(keptClauses)
                        keptClausesCount = keptClausesCount + keptClauses.size

                        // dont add dups

                      }

                      case _ => {
                        log.debug("Duplicate clause have been delted")
                      }
                    }


                  }
                  case _ => {
                    log.debug("No clauses lef after Tautology Deletion and self subsumpion")
                  }


                }

              }

              case _ => {
                log.debug("Nothing  has been derived given clause : {}", given)
              }


            }


          }


        }

        case _ => {
          log.warn("Given clause was empty , this does not seem right")
        }
      }

      iteration += 1

      if (iteration % 50 == 0) {
        log.info("Iteration : {}  WorkedOff size :  {}", iteration, workedOff.size)
        log.info("Iteration : {}  Usable size :  {}", iteration, usable.size)
        log.info("Iteration : {}  Derived Clauses :  {}", iteration, derivedClausesCount)
        log.info("Iteration : {}  Kept Clauses :  {}", iteration, keptClausesCount)
        val runtime: Double = System.currentTimeMillis - startTime
        log.info("Derived Clauses Per Second : {}  ", derivedClausesCount / (runtime / 1000), keptClausesCount)
      }
    }

    if (containsEmptyClause(usable)) {
      log.info("Proof found")
      log.info("WorkedOff size :  {}", workedOff.size)
      log.info("Derived Clauses Count :  {}", derivedClausesCount)
      log.info("Kept Clauses Are :  {}", workedOff)
      ProofFound()
    } else if (usable.isEmpty) {
      log.info("Completion found")
      log.info("WorkedOff size :  {}", workedOff.size)
      log.info("Derived Clauses Count :  {}", derivedClausesCount)
      log.info("Kept Clauses Are :  {}", workedOff)
      CompletionFound()
    } else {
      log.error("Some error occured during prooving, there has been no prooving result")
      error("Some error occured during prooving, there has been no prooving result")
    }


  }

  private def printOutPrecedence(clauses: List[FOLClause]) {
    //val literals : List[FOLNode] = clauses.map


  }


  def removeDups2(fresh: ClauseStorage, usable: ClauseStorage, workedOff: ClauseStorage): ClauseStorage = {
    fresh

  }

  def removeDups(fresh: ClauseStorage, usable: ClauseStorage, workedOff: ClauseStorage): ClauseStorage = {
    // interreduce all claueses
    val buffer = new ListBuffer[FOLClause]
    for (c <- fresh;
         if (usable.contains(c) || workedOff.contains(c)))
      {
        buffer.append(c)
      }

    val dups = buffer.toList
    // return clauses that where not duplicates
    fresh.filterClauses {dups.contains(_)}


  }


  def containsEmptyClause(clauses: Iterable[FOLClause]) = {
    (clauses exists ((_ match {
      case EmptyClause() => true
      case _ => false
    })))


  }

  def resolve(a: FOLClause, b: ClauseStorage): ClauseStorage = {
    b match {
      case NonEmptyClauseStore(clauses) => {
        val resolved = resolver.resolve(a, clauses)
        log.debug("Resolved : {}", resolved)
        resolved
      }
      case _ => CNFClauseStore()
    }


  }

  def choose(clauses: ClauseStorage): ClauseStorage = {

    val choosen = clauses(rnd.nextInt(clauses.size))
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