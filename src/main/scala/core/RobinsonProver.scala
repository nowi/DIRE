package core


import collection.mutable.{ListBuffer}
import domain.fol.ast._
import helpers.{Subject, Logging}
import kernel.{GivenClause, DerivedBatch, Derived}
import ProvingResult._
import collection.immutable.{TreeSet, SortedSet}
import containers._
import formatting.ClauseFormatting
import heuristics.LightestClauseHeuristicStorage
import net.lag.configgy.Configgy
import ordering.LiteralComparison
import recording.{EventRecorder, ClauseRecording}
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
  val inferenceRecorder: Option[ClauseRecording];
  val eventRecorder: Option[EventRecorder];
  val subsumptionStrategy: Subsumption;
  val forwardSubsumer: ForwardSubsumption;
  val backwardSubsumer: BackwardSubsumption;
  val resolver: Resolution;
  val positiveFactorer: PositiveFactoring;
  val negativeFactorer: NegativeFactoring;
  val timeLimit: Long;
  val isDistributed: Boolean})
        extends FOLProving
                with Logging with ClauseFormatting {
  implicit def setofFOLNode2ALCDClause(literals: Set[FOLNode]): ALCDClause = ALCDClause(literals)

  implicit def FOLClause2SetFOLNode(clause: FOLClause): Set[FOLNode] = clause.literals
  // reduction + subsumption functions

  //
  val isDistributed = env.isDistributed
  val timeLimit = env.timeLimit
  val resolver = env.resolver
  val positveFactorer = env.positiveFactorer
  val negativeFactorer = env.negativeFactorer

  val factoringEnabled = true
  val enableSharedClauses = false
  val seenClauses = new ListBuffer[FOLClause]()

  val recordClauses = true
  val backReductionOnRecievedClauses = true
  val forwardReductionOnRecievedClauses = true

  //implicit val literalComperator = env.literalComparator
  implicit val subsumptionChecker = env.subsumptionStrategy



  // done
  val forwardSubsumer = env.forwardSubsumer
  val tautologyDetection = ClauseTautologyDetector.apply _
  val condensation = ClauseCondenser.apply _
  val trivialLiteralDeletion = TrivialLiteralDeleter.apply _

  //val duplicateLiteralDeleter = DuplicateLiteralDeleter.apply _

  val backwardSubsumer = env.backwardSubsumer


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
  // take note this are MUTABLE structures that will be mutated by multiple methods in this class
  private val _usable: MutableClauseStorage = env.usableClauseStore
  private val _workedOff: MutableClauseStorage = env.workedOffClauseStore


  implicit val _inferenceRecorder = env.inferenceRecorder
  val eventRecorder = env.eventRecorder

  var derivedClausesCount: Int = 0
  var keptClausesCount: Int = 0
  var recievedClauseCount: Int = 0
  var recievedKeptClauseCount: Int = 0




  var iteration = 1;
  val startTime = System.currentTimeMillis
  var resolvedEmptyClause = false


  // this can be called from upper system layer if they need to integrate a claus
  // used only from the proving actor when a clause stays in this node in the distributed case
  // TODO this is not very clean from a design perspective fix this in the future
  override def addToUsable(clause: FOLClause) = {
    _usable.add(clause)
  }


  override def addAllToUsable(clauses: Iterable[FOLClause]) = {
    _usable.addAll(clauses)
  }

  override def addToWorkedOff(clause: FOLClause) = {
    _workedOff.add(clause)
  }


  override def addAllToWorkedOff(clauses: Iterable[FOLClause]) = {
    _workedOff.addAll(clauses)
  }


  override def inferenceLog = _inferenceRecorder

  override def workedOff = _workedOff.toList

  override def saturate(clauses: Iterable[FOLClause]) = {
    clauses match {
      case Nil => {
        log.warning("Cannot staurate empty clauses ... ignore")
        (COMPLETION, _workedOff.toList.size, derivedClausesCount, recievedClauseCount, recievedKeptClauseCount)
      }

      case clauses if (_workedOff.toList.isEmpty) => {
        // this is the first saturation , we have no background clauses
        log.warning("%s is starting first saturation , we have no background clauses", this)

        // record those clauses

        if (recordClauses) {
          _inferenceRecorder match {
            case Some(ir) => {
              for (clause <- clauses) {
                ir.recordClause(clause, None, None)
                //              log.warning("Input : %s", printInferenceStep(clause))
              }
            }

            case None => // no inference recorder present
          }
        }

        if (recordClauses) {
          eventRecorder match {
            case Some(recorder) => {
              for (clause <- clauses) {
                recorder.recordInputClause(clause)
              }
            }

            case None => // no inference recorder present
          }
        }


        // print input clauses
        //for(clause <- clauses)(log.warning("Input : %s",printInferenceStep(clause)))


        // first pass the clauses through the rewriting rules ( these are potentialy destructive )
        val rewritten = clauses.map(forwardLiteralDeletion(_))

        // filter out tautologies each redundant clause
        val filtered = clauses.filter(!tautologyDetection(_))



        doSaturate(filtered)
      }

      case clauses if (!_workedOff.isEmpty) => {
        log.debug("%s is restarting saturation", this)
        // TODO interreducate with workedoff

        // count recived

        recievedClauseCount += clauses.toList.size

        // record those clauses
        if (recordClauses) {
          _inferenceRecorder match {
            case Some(ir) => {
              for (clause <- clauses) {
                ir.recordRecievedClause(clause, None, None)
                //              log.warning("Recieved Input : %s", printInferenceStep(clause))
              }
            }

            case None => // no inference recorder present
          }

        }

        // first check if we do forward reduce the recieved clauses
        val interReduced = if (forwardReductionOnRecievedClauses) {
          // first pass the clauses through the rewriting rules ( these are potentialy destructive )
          val rewritten = clauses.map(forwardLiteralDeletion(_))
          // filter out tautologies each redundant clause
          rewritten.filter(!forwardRedundancyCheck(_, _workedOff)).map(ALCDClause(_))
        } else clauses



        if (interReduced.isEmpty) {
          log.info("Recived redundant clause .. exit")
          (COMPLETION, _workedOff.toList.size, derivedClausesCount, recievedClauseCount, recievedKeptClauseCount)
        } else if (backReductionOnRecievedClauses) {
          // remainig clauses are then used for backward reduction on usable and workedoff sets
          // backward subsumption on usable
          //val backwardSubsumed = interReduced.flatMap({clause => backwardSubsumer.apply(clause,usable)})
          _workedOff.removeAll(interReduced.flatMap({clause => backwardSubsumer(clause, _workedOff)}))

          recievedKeptClauseCount += interReduced.toList.size

          doSaturate(interReduced)

        } else {
          recievedKeptClauseCount += interReduced.toList.size
          doSaturate(clauses)

        }


      }

    }
  }

  private def doSaturate(clauses: Iterable[FOLClause]) = {
    // submethods

    _usable.addAll(clauses)

    while (_usable.hasNext && !resolvedEmptyClause && !isTimeUp(startTime)) {
      //record.info("Inner Loop")
      // 5. select a clause
      // sort the list and remove duplicates
      val givenClause: FOLClause = _usable.removeNext

      //TODO remove this
      // check if we already know this clause
      //      if(seenClauses.toList.contains(givenClause)) {
      //        log.error("WE ALREADY KNOW THIS CLAUSE %s",givenClause)
      //      } else {
      //        seenClauses += givenClause
      //      }
      //

      // add given to workedoff
      _workedOff.add(givenClause)

      // dispatch given clause , maybe it does not belong to this partition
      //notify listeners with kept clause
      notifyObservers(GivenClause(givenClause))

      // infere -- this should give us back clause buffers
      val successfullResolutions: Iterable[SuccessfullResolution] = infere(givenClause, _workedOff)

      successfullResolutions match {
        case successfullResolutions if (!successfullResolutions.isEmpty) => {
          successfullResolutions.exists(_.result.isEmpty) match {
            case true => { // we have indeed derived the empty clause
              log.warning("we have derived some empty clause , is this right ?")
              resolvedEmptyClause = true
              // TODO send empty clause to all reasoners

            }
            case false => { // we have derived clauses other than the empty clause
              // log here total derived
              if (recordClauses) {
                _inferenceRecorder match {
                  case Some(inferenceRecorder) => {
                    successfullResolutions.foreach({
                      r: core.resolution.SuccessfullResolution =>
                              inferenceRecorder.recordClause(SharedALCDClause(r.result), Some(SharedALCDClause(r.parent1)), Some(SharedALCDClause(r.parent2)))
                    })
                  }

                  case None => // no inference recorder present
                }
                eventRecorder match {
                  case Some(recorder) => {
                    successfullResolutions.foreach({
                      r: core.resolution.SuccessfullResolution =>
                              recorder.recordDerivedClause(SharedALCDClause(r.result), SharedALCDClause(r.parent1),SharedALCDClause(r.parent2))
                    })
                  }

                  case None => // no inference recorder present
                }


              }
              // interreduce
              interReduce(successfullResolutions.map(_.result), _usable, _workedOff)


            }

          }
        }

        case _ => {
          // there werent any successfull resolutions

        }
      }


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

    }


    if (resolvedEmptyClause) {
      log.error("%s Proof found", this)
      log.info("WorkedOff size :  %s", _workedOff.toList.size)
      log.info("Derived Clauses Count :  %s", derivedClausesCount)
      log.info("Kept Clauses Are :  %s", _workedOff)
      (PROOF, _workedOff.toList.size, derivedClausesCount, recievedClauseCount, recievedKeptClauseCount)
    } else if (!_usable.hasNext) {
      log.error("%s Completion found", this)
      log.info("WorkedOff size :  %s", _workedOff.size)
      //log.warning("%s", formatClauses(workedOff))
      //      log.warning("%s", workedOff.toList.map("%s\n" format _).sort(_ < _))
      //      log.info("Kept Clauses Are :  %s", workedOff.toList.map("%s \n" format _))
      (COMPLETION, _workedOff.toList.size, derivedClausesCount, recievedClauseCount, recievedKeptClauseCount)
    } else {
      log.error("%s Error found", this)
      (ERROR, _workedOff.toList.size, derivedClausesCount, recievedClauseCount, recievedKeptClauseCount)
    }


  }


  def infere(givenClause: FOLClause, workedOff: ClauseStorage) = {
    // extract the information we get back from resolvers , take care about the logging dispatching
    // here or shortcircuit if logging disabled, check performance implications

    // check which type of resolver we are usign and extract the derived clauses acorrdingly
    // binary resolvers always reutrn single clauses

    val successfullResolutions: Iterable[SuccessfullResolution] = resolver match {
      case binaryResolver: BinaryResolution => {
        binaryResolver(givenClause, workedOff).filter(_.isInstanceOf[SuccessfullResolution]).asInstanceOf[Iterable[SuccessfullResolution]]
      }

      case defaultResolver: Resolution => {
        //        defaultResolver(givenClause, workedOff).flatMap(_.result.asInstanceOf[List[Set[FOLNode]]])
        defaultResolver(givenClause, workedOff).filter(_.isInstanceOf[SuccessfullResolution]).asInstanceOf[Iterable[SuccessfullResolution]]
      }
    }

    // return only the successfull resolutions
    successfullResolutions

    // TODO fix this to work with resolution result structures too indtead only with term lists
    // perform positive factoring on resolved clauses

    //    factoringEnabled match {
    //      case true => {
    //        val factoredResolutions = successfullResolutions.map(positveFactorer(_)).map(negativeFactorer(_))
    //
    //        val factoredGivenClause = positveFactorer(negativeFactorer(givenClause)) match {
    //          case factored if (factored.toList.size < givenClause.size) => List(factored)
    //          case _ => Nil
    //        }
    //
    //        val factored = factoredResolutions ++ factoredGivenClause
    //
    //        factored
    //      }
    //
    //      case false => {
    //        successfullResolutions
    //      }
    //    }


  }


  def interReduce(clauses: Iterable[Set[FOLNode]], usable: MutableClauseStorage, workedOff: MutableClauseStorage) {
    // create store for newClauses with lightest clause heurstic

    val derivedClauseList = clauses.toList

    derivedClausesCount = derivedClausesCount + derivedClauseList.size

    val newClauses = new MutableClauseStore with LightestClauseHeuristicStorage
    newClauses.addAll(Set() ++ derivedClauseList.map(ALCDClause(_)))





    def forwardReduction(clause: Set[FOLNode]): Set[FOLNode] = {
      tautologyDetection(clause) match {
        case true => Set.empty[FOLNode] // clause is taut
        case false => {
          // first pass the clauses through the rewriting rules ( these are potentialy destructive )
          val rewritten = forwardLiteralDeletion(clause)
          // now check if redundant
          if (forwardRedundancyCheck(rewritten, workedOff, usable)) {
            Set.empty[FOLNode]
          } else {
            rewritten
          }
        }
      }
    }


    def bSub(clause: Set[FOLNode]) = {
      // reduce mutable stores new, workedoff and usable with clause
      // reduced clauses are added into new store
      //        val newReduced = backwardSubsumer(clause, newClauses)
      //        newReduced

      val workedOffReduced = Set() ++ backwardSubsumer(clause, workedOff)
      if (!workedOffReduced.isEmpty)
        workedOff.removeAll(workedOffReduced)

      val usableReduced = Set() ++ backwardSubsumer(clause, usable)
      if (!usableReduced.isEmpty)
        usable.removeAll(usableReduced)

      if (!usableReduced.isEmpty || !workedOffReduced.isEmpty)
        newClauses.addAll(Set() ++ workedOffReduced ++ usableReduced)
    }


    val keptClauses = new ListBuffer[FOLClause]()
    while (newClauses.hasNext) {
      // forward reduction on given

      val nextClause = newClauses.removeNext

      val given = forwardReduction(nextClause)
      if (!given.isEmpty) {
        // backward subsumption on new, usable , and workedoff


        bSub(given)
        val keptClause = if (enableSharedClauses) SharedALCDClause(given) else ALCDClause(given)

        //log.warning("Kept : %s", printInferenceStep(keptClause))

        keptClausesCount = keptClausesCount + 1


        // buffer the kept clause
        keptClauses.append(keptClause)

      }
    }


    if (!keptClauses.isEmpty) {
      if (isDistributed) {
        //notify listeners with kept clause
        // if this clause stays in this node then the dispatcher will
        // add it into the usable set
        notifyObservers(DerivedBatch(keptClauses))
        //          usable.add(keptClause)

      } else {
        // we not working distributed , add directly into the usable set
        usable.addAll(keptClauses)
      }

    }


  }


  def forwardLiteralDeletion(clause: Set[FOLNode]): Set[FOLNode] = {
    //condensation(duplicateLiteralDeleter(clause))(subsumptionChecker)
    clause
  }


  def forwardRedundancyCheck(clause: Set[FOLNode], backgroundClauses: ClauseStorage): Boolean = {
     forwardSubsumer(clause, backgroundClauses)
  }

  // rewritten in functional style
  def forwardRedundancyCheck(clause: Set[FOLNode], workedOff: ClauseStorage, usable: ClauseStorage): Boolean = {
    // filtering phase
    val isRedundand = tautologyDetection(clause) || forwardSubsumer(clause, usable) || forwardSubsumer(clause, workedOff)

    val alcdclause: ALCDClause = clause



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


  private def recordClauses(clauses: Iterable[FOLClause]) {
    //    record all initiial clauess
    _inferenceRecorder match {
      case Some(inferenceRecorder) => {
        for (clause <- clauses)
          inferenceRecorder.recordClause(clause, None, None)
      }

      case None => // no inference recorder present
    }


  }

  private def recordRecievedClauses(clauses: Iterable[FOLClause]) {
    // record all initiial clauess
    _inferenceRecorder match {
      case Some(inferenceRecorder) => {
        for (clause <- clauses)
          inferenceRecorder.recordRecievedClause(clause, None, None)
      }

      case None => // no inference recorder present
    }


  }


}