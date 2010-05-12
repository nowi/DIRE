//package core
//
//
//import helpers.{Subject, Logging}
//import ProvingResult._
//import collection.immutable.{TreeSet, SortedSet}
//import collection.mutable.ListBuffer
//import containers._
//import domain.fol.ast._
//import formatting.ClauseFormatting
//import ordering.LiteralComparison
//import ClauseRecording
//import resolution.Resolution
//
//
///**
// * User: nowi
// * Date: 21.10.2009
// * Time: 15:20:54
// *
// * A first Resolution based prover
// *
// * Mirrors the example prover from Chapter 27 - Combining superposition sorts and splitting
// * from the Automated Theorem Proving Book Volume II
// */
//
//
//trait FOLProving extends Subject {
//  def entail(clause: FOLClause): (ProvingResult, ClauseStorage)
//
//  def saturate(): (ProvingResult, ClauseStorage)
//
//  /**
//   * Saturation without background knowledge
//   */
//  def saturate(clauses: ClauseStorage): (ProvingResult, ClauseStorage)
//
//  /**
//   * Saturation with background knowledge
//   */
//  def saturate(clauses: ClauseStorage, backgroundClauses: ClauseStorage): (ProvingResult, ClauseStorage)
//
//}
//
//
//
//
//class ResolutionProover1(env: {
//  val initialClauses: ClauseStorage;
//  val recordProofSteps: Boolean;
//  val inferenceRecorder : ClauseRecording;
//  val tautologyDeleter: TautologyDeletion;
//  val factorizer: Factoring;
//  val resolver: Resolution;
//  val removeDuplicates: Boolean;
//  val usableBackSubsumption: Boolean;
//  val forwardSubsumption: Boolean;
//  val literalComparator: LiteralComparison;
//  val timeLimit: Long;
//  val dropSeenClauses: Boolean;
//  val useLightesClauseHeuristic: Boolean})
//        extends FOLProving
//                with Logging with ClauseFormatting {
//
//
//
//  val initialClauses = env.initialClauses
//
//  val rnd = new Random(System.currentTimeMillis)
//
//
//  val resolver = env.resolver
//  val tautologyDeleter = env.tautologyDeleter
//  val subsumptionDeleter = env.subsumptionDeleter
//  val factorizer = env.factorizer
//  val literalComparator = env.literalComparator
//
//  val removeDuplicates = env.removeDuplicates;
//  val useLightesClauseHeuristic = env.useLightesClauseHeuristic
//  val usableBackSubsumption = env.usableBackSubsumption
//  val forwardSubsumption = env.forwardSubsumption
//  val dropSeenClauses = env.dropSeenClauses
//
//  val inferenceRecorder = env.inferenceRecorder;
//  val recordProofSteps = env.recordProofSteps
//
//  val timeLimit: Long = env.timeLimit;
//
//
//  override def entail(clause: FOLClause) = {
//    log.info("Configuration is  %s", env)
//    log.info("Starting entailment proof on clause store %s", initialClauses)
//    saturate(clause :: initialClauses)
//  }
//
//  override def saturate() = {
//    log.info("Configuration is  %s", env)
//    saturate(initialClauses)
//  }
//
//
//  def saturate(clauses: ClauseStorage) = {
//    saturate(clauses, CNFClauseStore())
//  }
//
//  override def saturate(clauses: ClauseStorage, backgroundClauses: ClauseStorage) = {
//    // init time
//
//    if(backgroundClauses.size > 0) {
//      log.info("Restrating saturation with background knowledge")
//    }
//    var usable: UsableClauseStore = new UsableClauseStore(List[FOLClause]())
//    var workedOff: ClauseStorage = new WorkedOffClauseStore
//
//    var seenClauses = Set[FOLClause]()
//
//
//    // all candidate clauses to generate inferences
//    // perform input reduction
//    //var usable: OrderedSet[FOLClause] = taut(sub(clauses))
//    workedOff = workedOff ::: backgroundClauses
//
//
//    // tatology deletion and self subsumptoin removal on usable
//    if(recordProofSteps){
//      // TODO this will loose history
//      // register all clauses in record
//      backgroundClauses.foreach(inferenceRecorder.recordClause(_,None,None))
//      clauses.foreach(inferenceRecorder.recordClause(_,None,None))
//
//    }
//    //record.debug("Starting saturation of clause store %s", formatClauseStore(clauses,false))
//    // tautology reduction and self subsumption on new clauses
//    var cleaned = taut(sub(clauses))
//
//    // forward reducation based on background clauses
//    // TODO REFACTOR THIS
//    cleaned = workedOff match {
//      case NonEmptyClauseStore(clauses) if (forwardSubsumption) => sub(cleaned, workedOff)
//      case _ => cleaned
//    }
//    // add the cleaned into the usable store
//    usable.enqueue(cleaned)
//
//
//
//    var derivedClausesCount: Int = 0
//    var keptClausesCount: Int = 0
//    var iteration = 1;
//    val startTime = System.currentTimeMillis
//    while (!usable.isEmpty && !containsEmptyClause(usable) && !isTimeUp(startTime)) {
//      //record.info("Inner Loop")
//      // 5. select a clause
//      // sort the list and remove duplicates
//      val headClause: FOLClause = usable.head
//      usable = usable.tail
//
//
//      CNFClauseStore(headClause) match {
//        case NonEmptyClauseStore(given) => {
//          log.debug("Current Given clause is  %s", given)
//
//
//          // get the maxLiteral of given
//
//          val maxLit = headClause.maxLits(literalComparator)
//
//          log.trace("Maximum literal of given Clause %s is %s", headClause, maxLit)
//
//
//
//          if (seenClauses.contains(headClause) && dropSeenClauses) {
//            log.warning("Already have seen this clause : %s.. dropping it", headClause)
//          } else {
//
//            // 5. 6. add to workedoff, remove from usable       fa
//            workedOff = given ::: workedOff
//            seenClauses += headClause
//            log.debug("After 5. 6.  usable : %s", usable)
//            log.debug("After 5. 6.  workedOff : %s", workedOff)
//            //Hydrophilic(U) -> Hydrophobicity(U)*.
//
//
//            // 7. all resolution inference conlusions between given and workedoff and all
//            // factoring inference conclusions from given are stored in fresh
//            val resolved = resolve(headClause, workedOff) //++ factor(given)
//
//
//
//            // we have some derived clauses
//            log.debug("We have successfully derived claueses %s", resolved)
//
//            // 8. - 11.  Perform reductions/forward contractions
//            // remove all tautologies and subsumptions from fresh
//
//
//            resolved match {
//              case NonEmptyClauseStore(derivedClauses) => {
//                derivedClausesCount = derivedClausesCount + derivedClauses.size
//                // self subsumption and tautology remoal
//                taut(sub(derivedClauses)) match {
//                  case NonEmptyClauseStore(newClause) => {
//                    // first kill clauses that are already in workedoff or usable
//                    removeDups2(newClause, usable, workedOff) match {
//                      case NonEmptyClauseStore(uniqueDerived) => {
//                        // forward subsupmtion on fresh
//
//                        val keptClauses: ClauseStorage = forwardSubsumption match {
//                          case true => {
//                            sub(sub(uniqueDerived, workedOff), usable) // TODO FIX THIS
//                          }
//                          case false => uniqueDerived
//                        }
//
//                        log.trace("After Forward Subsumption Fresh Clause %s", keptClauses)
//
//
//                        // clasuse remaining in fresh are then used for backward subsumtion
//                        workedOff = usableBackSubsumption match {
//                          case true => sub(workedOff, keptClauses)
//                          case false => workedOff
//                        }
//
//                        log.trace("After Backward Subsumption workedOff Clauses : %s", workedOff)
//
//                        // backwardsubsumption on usable based on fresh clause
//                        // TODO REENABLE BACKWARD SUBSUMPTION WITH INDEX SUPPORT
//
//
//                        if (recordProofSteps) {
//                          log.info(formatClauseStore(keptClauses, true))
//                        }
//
//                        val unique: ClauseStorage = CNFClauseStore(keptClauses.clauses.filter({x : FOLClause => !usable.contains(x) && !workedOff.contains(x)}))
//                        if (unique != keptClauses) {
//                          log.debug("!! We have derived clauses that are already contained in usable or workedoff")
//                          log.debug("this is poor mans backward subs till index support is added")
//                        }
//                        if (unique.size > 0) {
//                          usable.enqueue(unique)
//                          // notify listeners
//                          notifyObservers(keptClauses)
//                          keptClausesCount = keptClausesCount + keptClauses.size
//                        }
//
//
//                        // dont add dups
//
//                      }
//
//                      case _ => {
//                        log.debug("Duplicate clause have been delted")
//                      }
//                    }
//
//
//                  }
//                  case _ => {
//                    log.debug("No clauses lef after Tautology Deletion and self subsumpion")
//                  }
//
//
//                }
//
//              }
//
//              case _ => {
//                log.debug("Nothing  has been derived given clause : %s", given)
//              }
//
//
//            }
//
//
//          }
//
//
//        }
//
//        case _ => {
//          log.warning("Given clause was empty , this does not seem right")
//        }
//      }
//
//      iteration += 1
//
//      if (iteration % 50 == 0) {
//        log.debug("Iteration : %s  WorkedOff size :  %s", iteration, workedOff.size)
//        log.debug("Iteration : %s  Usable size :  %s", iteration, usable.size)
//        log.debug("Iteration : %s  Derived Clauses :  %s", iteration, derivedClausesCount)
//        log.debug("Iteration : %s  Kept Clauses :  %s", iteration, keptClausesCount)
//        val runtime: Double = System.currentTimeMillis - startTime
//        log.debug("Derived Clauses Per Second : %s  ", derivedClausesCount / (runtime / 1000), keptClausesCount)
//      }
//    }
//
//    if (containsEmptyClause(usable)) {
//      log.info("Proof found")
//      log.info("WorkedOff size :  %s", workedOff.size)
//      log.info("Derived Clauses Count :  %s", derivedClausesCount)
//      log.info("Kept Clauses Are :  %s", workedOff)
//      (PROOF, workedOff)
//    } else if (usable.isEmpty) {
//      log.info("Completion found")
//      log.info("WorkedOff size :  %s", workedOff.size)
//      log.info("Derived Clauses Count :  %s", derivedClausesCount)
//      log.info("Kept Clauses Are :  %s", workedOff)
//      (COMPLETION, workedOff)
//    } else {
//      // time up
//      log.info("Completion found")
//      log.info("WorkedOff size :  %s", workedOff.size)
//      log.info("Derived Clauses Count :  %s", derivedClausesCount)
//      log.info("Kept Clauses Are :  %s", workedOff)
//      (TIMEUP, workedOff)
//
//    }
//
//
//  }
//
//
//
//  private def printOutPrecedence(clauses: List[FOLClause]) {
//    //val literals : List[FOLNode] = clauses.map
//
//
//  }
//
//  def formatClauseStore(clauses : ClauseStorage,showParents : Boolean) : String = {
//    formatClauses(clauses,inferenceRecorder,showParents)
//  }
//
//
//  def isTimeUp(startingTime: Long): Boolean = {
//    if (timeLimit > 0) {
//      (System.currentTimeMillis - startingTime) > timeLimit
//    } else false
//  }
//
//
//  def removeDups2(fresh: ClauseStorage, usable: ClauseStorage, workedOff: ClauseStorage): ClauseStorage = {
//    fresh
//
//  }
//
//  def removeDups(fresh: ClauseStorage, usable: ClauseStorage, workedOff: ClauseStorage): ClauseStorage = {
//    // interreduce all claueses
//    val buffer = new ListBuffer[FOLClause]
//    for (c <- fresh;
//         if (usable.contains(c) || workedOff.contains(c)))
//      {
//        buffer.append(c)
//      }
//
//    val dups = buffer.toList
//    // return clauses that where not duplicates
//    fresh.filterClauses {dups.contains(_)}
//
//
//  }
//
//
//  def containsEmptyClause(clauses: Iterable[FOLClause]) = {
//    (clauses exists ((_ match {
//      case EmptyClause() => true
//      case _ => false
//    })))
//
//
//  }
//
//  def resolve(a: FOLClause, b: ClauseStorage): ClauseStorage = {
//    b match {
//      case NonEmptyClauseStore(clauses) => {
//        val resolved = resolver.resolve(a, clauses)
//        log.debug("Resolved : %s", resolved)
//        resolved
//      }
//      case _ => CNFClauseStore()
//    }
//
//
//  }
//
//  def choose(clauses: ClauseStorage): ClauseStorage = {
//
//    val choosen = clauses(rnd.nextInt(clauses.size))
//    log.trace("Naively choosing clause : %s from clauses store : %s", choosen, clauses)
//    CNFClauseStore(choosen)
//  }
//
//  def factor(clauses: ClauseStorage): ClauseStorage = {
//    factorizer.factorize(clauses)
//  }
//
//  def taut(clauses: ClauseStorage): ClauseStorage = {
//    tautologyDeleter(clauses)
//  }
//
//  def sub(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
//
//    val cleaned = subsumptionDeleter.deleteSubsumptions(a, b)
//    (a.size - cleaned.size) match {
//      case delta if(delta > 0) =>  log.info("Subsumption deletion removed %s clauses",delta)
//      case _ => //
//    }
//
//    cleaned
//
//  }
//
//
//  /**
//   * Apply subsumption deletion to all of the clauses in the clauseStore
//   * and return the resulting clausestore
//   */
//  def sub(a: ClauseStorage): ClauseStorage = {
//    if(containsEmptyClause(a.clauses))
//      a
//    else {
//      subsumptionDeleter.deleteSubsumptions(a)
//    }
//  }
//
//
//}
//
//
