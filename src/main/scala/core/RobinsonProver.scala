package core


import collection.mutable.{ListBuffer}
import domain.fol.ast._
import helpers.{Subject, Logging}
import kernel.Derived
import ProvingResult._
import collection.immutable.{TreeSet, SortedSet}
import containers._
import formatting.ClauseFormatting
import heuristics.LightestClauseHeuristicStorage
import net.lag.configgy.Configgy
import ordering.LiteralComparison
import recording.ClauseRecording
import reduction._
import resolution._
import scala.{Function => ScalaFun}

import ClauseStorage._

/**
 * User: nowi
 * Date: 22.04.2010
 * Time: 19:30:47
 */

class RobinsonProver(env: {
  val usableClauseStore: MutableClauseStorage;
  val workedOffClauseStore: MutableClauseStorage;
  val recordProofSteps: Boolean;
  val inferenceRecorder: ClauseRecording;
  val subsumptionStrategy: Subsumption;
  val forwardSubsumer: ForwardSubsumption;
  val backwardSubsumer: BackwardSubsumption;
  val resolver: Resolution;
  val positiveFactorer: PositiveFactoring;
  val timeLimit: Long})
        extends FOLProving
                with Logging with ClauseFormatting {
  implicit def listofFOLNode2FOLClause(literals: List[FOLNode]): FOLClause = ALCDClause(literals)

  implicit def listofFOLClause2ListFOLNode(clause: FOLClause): List[FOLNode] = clause.literals
  // reduction + subsumption functions

  //
  val timeLimit = env.timeLimit
  val resolver = env.resolver
  val positveFactorer = env.positiveFactorer


  val seenClauses = new ListBuffer[FOLClause]()

  //implicit val literalComperator = env.literalComparator
  implicit val subsumptionChecker = env.subsumptionStrategy



  // done
  val forwardSubsumer = env.forwardSubsumer
  val tautologyDetection = ClauseTautologyDetector.apply _
  val condensation = ClauseCondenser.apply _
  val trivialLiteralDeletion = TrivialLiteralDeleter.apply _
  val duplicateLiteralDeleter = DuplicateLiteralDeleter.apply _

  val backwardSubsumer = env.backwardSubsumer

  val inferenceRecorder = env.inferenceRecorder

  // TODO
  //val forwardRewriting = ForwardRewriter.apply _


  // TODO check which of those are relevant
  //  val assignmentEquationDeletion = ScalaFun.tupled(AssingmentEquationDeleter.apply _)
  //  val reductionsSortSimplification = ScalaFun.tupled(ReductionsSortSimplificator.apply _)
  //  val forwardClauseReduction = ScalaFun.tupled(ForwardClauseReducer.apply _)
  //  val unitConflictResolution = ScalaFun.tupled(UnitConflictResolver.apply _)
  //  val staticSoftTyping = ScalaFun.tupled(StaticSoftTyper.apply _)
  //
  //  val backwardSubsuptionElimination = ScalaFun.tupled(BackwardSubsumptionEliminator.apply _)
  //  val backwardMatchingReplacementResolution = ScalaFun.tupled(BackwardMatchingReplacementResolver.apply _)
  //  val backwardRewriting = ScalaFun.tupled(BackwardRewriter.apply _)


  // init the storeas
  val usable: MutableClauseStorage = env.usableClauseStore
  val workedOff: MutableClauseStorage = env.workedOffClauseStore

  var derivedClausesCount: Int = 0
  var keptClausesCount: Int = 0
  var iteration = 1;
  val startTime = System.currentTimeMillis
  var resolvedEmptyClause = false

  override def saturate(clauses: Iterable[FOLClause]) = {
    clauses match {
      case Nil => {
        log.info("Cannot staurate empty clauses ... ignore")
        (COMPLETION, workedOff.toList)
      }

      case clauses if (workedOff.toList.isEmpty) => {
        // this is the first saturation , we have no background clauses
        log.info("This is the first saturation , we have no background clauses")
        // first pass the clauses through the rewriting rules ( these are potentialy destructive )
        val rewritten = clauses.map(forwardLiteralDeletion(_))

        // filter out tautologies each redundant clause
        val filtered = clauses.filter(!tautologyDetection(_))



        doSaturate(filtered)
      }

      case clauses if (!workedOff.isEmpty) => {
        log.info("Restarting saturation")
        // TODO interreducate with workedoff
        // first pass the clauses through the rewriting rules ( these are potentialy destructive )
        val rewritten = clauses.map(forwardLiteralDeletion(_))

        // filter out tautologies each redundant clause
        val interReduced = rewritten.filter(!forwardRedundancyCheck(_, workedOff)).map(ALCDClause(_))



        if (interReduced.isEmpty) {
          log.info("Recived redundant clause .. exit")
          (COMPLETION, workedOff.toList)
        } else {

          // remainig clauses are then used for backward reduction on usable and workedoff sets
          // backward subsumption on usable
          //val backwardSubsumed = interReduced.flatMap({clause => backwardSubsumer.apply(clause,usable)})
          workedOff.removeAll(interReduced.flatMap({clause => backwardSubsumer(clause, workedOff)}))

          doSaturate(interReduced)
        }


      }
    }

  }

  private def doSaturate(clauses: Iterable[FOLClause]) = {
    // submethods



    def interReduce(clauses: Iterable[List[FOLNode]]) {
      // create store for newClauses with lightest clause heurstic
      val newClauses = new MutableClauseStore with LightestClauseHeuristicStorage

      derivedClausesCount = derivedClausesCount + newClauses.size

      newClauses.addAll(clauses.map(ALCDClause(_)))



      def forwardReduction(clause: List[FOLNode]): List[FOLNode] = {
        // first pass the clauses through the rewriting rules ( these are potentialy destructive )
        val rewritten = forwardLiteralDeletion(clause)
        // now check if redundant
        if (forwardRedundancyCheck(rewritten, workedOff, usable)) {
          Nil
        } else {
          rewritten
        }
      }


      def bSub(clause: List[FOLNode]) = {
        // reduce mutable stores new, workedoff and usable with clause
        // reduced clauses are added into new store
//        val newReduced = backwardSubsumer(clause, newClauses)
//        newReduced

        val workedOffReduced = backwardSubsumer(clause, workedOff)
        workedOff.removeAll(workedOffReduced)

        val usableReduced = backwardSubsumer(clause, usable)
        usable.removeAll(usableReduced)

        newClauses.addAll(workedOffReduced ++ usableReduced)
      }



      while (newClauses.hasNext) {
        // forward reduction on given
        val given = forwardReduction(newClauses.removeNext)
        if (!given.isEmpty) {
          // backward subsumption on new, usable , and workedoff
          bSub(given)
          // bmrr
          // brew
          //finall add the given clause to usable

          // integrate into shared structures
//          val keptClause = ALCDClause(given)
          val keptClause = SharedALCDClause(given)

          //                log.debug("Given clause : %s", givenClause)
          //                for (newClause <- keptClauses if (!newClause.isEmpty)) {
          //                  //                  log.info(printInferenceStep(newClause, inferenceRecorder))
          //                  log.debug("Kept : %s", newClause)
          //
          //                }

          keptClausesCount = keptClausesCount + 1

          //notify listeners with kept clause
          notifyObservers(Derived(keptClause, None, None))

          usable.add(keptClause)
        }
      }

    }


    usable.addAll(clauses)

    // record all initiial clauess

    //    for (clause <- usable)
    //      inferenceRecorder.recordClause(clause, None, None)

    while (usable.hasNext && !resolvedEmptyClause && !isTimeUp(startTime)) {
      //record.info("Inner Loop")
      // 5. select a clause
      // sort the list and remove duplicates
      val givenClause: FOLClause = usable.removeNext

      //TODO remove this
      // check if we already know this clause
      //      if(seenClauses.toList.contains(givenClause)) {
      //        log.error("WE ALREADY KNOW THIS CLAUSE %s",givenClause)
      //      } else {
      //        seenClauses += givenClause
      //      }
      //

      // add given to workedoff
      workedOff.add(givenClause)

      // infere -- this should give us back clause buffers
      val newClauses: Iterable[List[FOLNode]] = infere(givenClause, workedOff)

      // check if the clause is not the empty clause
      newClauses.find(_.size == 0) match {
        case Some(emptyClause) => {
          log.warning("we have derived some empty clause , is this right ?")
          resolvedEmptyClause = true
        }
        case None => {
          // interreduce
          if (!newClauses.isEmpty) {
            // convert new clauses into shared structure
            interReduce(newClauses.map(SharedALCDClause(_)))
          }

        }

      }

      // convert to shared clause and integrate into usable clause store

      iteration += 1

      if (iteration % 1000 == 0) {
        //        log.info("Iteration : %s  WorkedOff size :  %s", iteration, workedOff.size)
        //        log.info("Iteration : %s  Usable size :  %s", iteration, usable.size)
        log.info("Iteration : %s  Derived Clauses :  %s", iteration, derivedClausesCount)
        log.info("Iteration : %s  Kept Clauses :  %s", iteration, keptClausesCount)
        val runtime: Double = System.currentTimeMillis - startTime
        log.info("Runtime (sec): %s", (runtime / 1000))
        log.info("Derived Clauses Per Second : %s  ", derivedClausesCount / (runtime / 1000), derivedClausesCount)
        log.info("Kept Clauses Per Second : %s  ", keptClausesCount / (runtime / 1000), keptClausesCount)
      }

      if (iteration % 5000 == 0) {
        //        log.info("Current Kept Clauses Are :  %s", workedOff.toList.map("%s \n" format _))
      }
    }


    if (resolvedEmptyClause) {
      log.info("Proof found")
      log.info("WorkedOff size :  %s", workedOff.size)
      log.info("Derived Clauses Count :  %s", derivedClausesCount)
      log.info("Kept Clauses Are :  %s", workedOff)
      (PROOF, workedOff.toList)
    } else if (!usable.hasNext) {
      log.info("Completion found")
      log.info("WorkedOff size :  %s", workedOff.size)
      log.info("Derived Clauses Count :  %s", derivedClausesCount)
      log.info("Kept Clauses Are :  %s", workedOff)
      (COMPLETION, workedOff.toList)
    } else {
      error("Should not be here")
    }


  }

  // TODO need clause buffer datastructure that should be used during forward reduction
  // reduction procedure operating directly on clause buffers , SIDEEFFECTS !
  //  def interReduce(newClauses: Iterable[List[FOLNode]], workedOff: MutableClauseStorage, usable: MutableClauseStorage): Iterable[List[FOLNode]] = {
  //    // first pass the clauses through the rewriting rules ( these are potentialy destructive )
  //    val rewritten = newClauses.map(forwardLiteralDeletion(_))
  //
  //    // now filter out all redundant clauses
  //    val interReduced = rewritten.filter(!forwardRedundancyCheck(_, workedOff, usable))
  //
  //    // remainig clauses are then used for backward reduction on usable and workedoff sets
  //    if (!interReduced.isEmpty) {
  //      // backward subsumption on usable
  //      //val backwardSubsumed = interReduced.flatMap({clause => backwardSubsumer.apply(clause,usable)})
  //      usable.removeAll(interReduced.flatMap({clause => backwardSubsumer(clause, usable)}))
  //
  //      workedOff.removeAll(interReduced.flatMap({clause => backwardSubsumer(clause, workedOff)}))
  //
  //
  //    }
  //    interReduced
  //    // TODO this needs to be removeed
  //  }




  //  def selfSubsume(clauses: Iterable[List[FOLNode]]): Iterable[List[FOLNode]] = {
  //    // check which clause is not subsumed by another clause
  //    val subsumed = for (subsumer <- clauses; subsumee <- clauses; if (subsumer != subsumee && subsumptionChecker(subsumer, subsumee))) yield subsumee
  //    // remove all that have been subsumed
  //    clauses -- subsumed
  //
  //  }


  def infere(givenClause: FOLClause, workedOff: ClauseStorage): Iterable[List[FOLNode]] = {
    // extract the information we get back from resolvers , take care about the logging dispatching
    // here or shortcircuit if logging disabled, check performance implications

    // check which type of resolver we are usign and extract the derived clauses acorrdingly
    // binary resolvers always reutrn single clauses

    // TODO check this , maybe just swo
    val successfullResolutions = resolver match {
      case binaryResolver: BinaryResolution => {

        val success = binaryResolver(givenClause, workedOff).filter(_.isInstanceOf[SuccessfullResolution]).asInstanceOf[Iterable[SuccessfullResolution]]
        //success.foreach(log.info("%s",_))


        // log the successfull resolutions
        //        success.foreach({r: core.resolution.SuccessfullResolution => inferenceRecorder.recordClause(r.result, Some(r.parent1), Some(r.parent2))})
        success.map(_.result.asInstanceOf[List[FOLNode]])
      }

      case defaultResolver: Resolution => {
        defaultResolver(givenClause, workedOff).flatMap(_.result.asInstanceOf[List[List[FOLNode]]])

      }
    }


    // perform positive factoring on resolved clauses


    val factoredResolutions = successfullResolutions.map(positveFactorer(_))

    val factoredGivenClause = positveFactorer(givenClause) match {
      case factored if (factored.size < givenClause.size) => List(factored)
      case _ => Nil
    }

    factoredResolutions ++ factoredGivenClause


  }


  def forwardLiteralDeletion(clause: List[FOLNode]): List[FOLNode] = {
    duplicateLiteralDeleter(clause)
  }


  def forwardRedundancyCheck(clause: List[FOLNode], backgroundClauses: ClauseStorage): Boolean = {
    tautologyDetection(clause) || forwardSubsumer(clause, backgroundClauses)
  }

  // rewritten in functional style
  def forwardRedundancyCheck(clause: List[FOLNode], workedOff: ClauseStorage, usable: ClauseStorage): Boolean = {
    // filtering phase
    val isRedundand = tautologyDetection(clause) || forwardSubsumer(clause, usable) || forwardSubsumer(clause, workedOff)


    isRedundand

    // we disabled forward rewriting
    //            forwardRewriting(given, workedOff, usable) match {
    //              case (true, frewritten) => {
    //                given = tautologyDeletion(frewritten)
    //
    //                given match {
    //                  case EmptyClause() => given
    //                  case _ => {
    //                    given = trivialLiteralDeletion(given)
    //                    given = condensation(given)
    //
    //                    forwardSubsumption(given, workedOff, usable) match {
    //                      case true => EmptyClause
    //                      case false => given
    //                    }
    //                  }
    //                }
    //              }
    //              case _ => given// there has been no forward rewriting
    //            }


    //        given = reductionsSortSimplification(given, workedOff, usable)
    //        given = forwardClauseReduction(given, workedOff, usable)
    //        given = unitConflictResolution(given, workedOff, usable)
    //        given = staticSoftTyping(given, workedOff, usable)


  }


  // this algorithm is modeled closely after the presented version int ATP vol 2 , page 2001
  // although its not really nice functional style..
  //  def forwardRedundancyCheck(clause: List[FOLNode], workedOff: ClauseStorage, usable: ClauseStorage): List[FOLNode] = {
  //    var given: List[FOLNode] = clause
  //    tautologyDetection(given) match {
  //      case true => Nil // is a tautolgy --> redundant
  //      case _ => {
  //        // no tautolgy , try further reductions , operate inplace on the FOLNode/LIteral buffer
  //        given = duplicateLiteralDeleter(given)
  //
  //        // TODO Trivialliteral deelter does not work correkt
  //       // given = trivialLiteralDeletion(given)
  //        //given = condensation(given)(subsumptionChecker)
  //        //given = assignmentEquationDeletion(given, workedOff, usable)
  //        forwardSubsumer(given, workedOff, usable) match {
  //          case true => Nil // clause is subsumed by a clause already in usable or workedoff
  //          case false => {
  //            given
  //          }
  //
  //        }
  //
  //
  //
  //        // we disabled forward rewriting
  //        //            forwardRewriting(given, workedOff, usable) match {
  //        //              case (true, frewritten) => {
  //        //                given = tautologyDeletion(frewritten)
  //        //
  //        //                given match {
  //        //                  case EmptyClause() => given
  //        //                  case _ => {
  //        //                    given = trivialLiteralDeletion(given)
  //        //                    given = condensation(given)
  //        //
  //        //                    forwardSubsumption(given, workedOff, usable) match {
  //        //                      case true => EmptyClause
  //        //                      case false => given
  //        //                    }
  //        //                  }
  //        //                }
  //        //              }
  //        //              case _ => given// there has been no forward rewriting
  //        //            }
  //
  //
  //        //        given = reductionsSortSimplification(given, workedOff, usable)
  //        //        given = forwardClauseReduction(given, workedOff, usable)
  //        //        given = unitConflictResolution(given, workedOff, usable)
  //        //        given = staticSoftTyping(given, workedOff, usable)
  //
  //      }
  //
  //    }
  //
  //  }

  def isTimeUp(startingTime: Long): Boolean = {
    if (timeLimit > 0) {
      (System.currentTimeMillis - startingTime) > timeLimit
    } else false
  }

}