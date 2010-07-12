package kernel


import actors.{Scheduler, LinkedQueue}
import collection.mutable.{ListBuffer, Queue, ArrayBuffer}
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent.TimeUnit
import java.util.Date
import recording.{EventRecording, ClauseRecording}
import se.scalablesolutions.akka.actor.{Scheduler, Actor}
import se.scalablesolutions.akka.dispatch.Dispatchers
import core._
import caches.URLitCache
import containers.{ClauseStorage, CNFClauseStore}
import domain.fol.ast.FOLClause
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Subject
import java.io.File
import core.reduction._
import core.rewriting.{VariableRewriter}
import ordering.{CustomConferencePartitionedPrecedence, CustomSPASSModule1Precedence, ALCLPOComparator}
import ProvingState._
import ProvingResult._
import core.selection.NegativeLiteralsSelection
import resolution.{UniqueLiteralResolution, SuccessfullResolution}
import se.scalablesolutions.akka.util.Logging
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 03.02.2010
 * Time: 17:11:44
 */


class ProvingActor(env: {val prover: FOLProving; val eventRecorder: Option[EventRecording]; val uniqueLiteralResolver: Option[UniqueLiteralResolution]; val uniqueRLitCache: URLitCache}) extends Actor with ReasoningActorChild {


  // cofigure a native os thread based dispatcher for the proving actor
//  val d = Dispatchers.newThreadBasedDispatcher(this)
//      val d = Dispatchers.globalReactorBasedSingleThreadEventDrivenDispatcher
  val d = Dispatchers.newExecutorBasedEventDrivenDispatcher("prover");
  d.withNewThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity
          .setCorePoolSize(5)
          .setMaxPoolSize(128)
          .setKeepAliveTimeInMillis(100)
          .setRejectionPolicy(new CallerRunsPolicy)
          .buildThreadPool;

  messageDispatcher = d

  val prover: FOLProving = env.prover

  val eventRecorder = env.eventRecorder

  prover.addObserver(this)


  val incomingClausesBuffer = new scala.collection.mutable.ListBuffer[Iterable[FOLClause]]()

  val MAILBOXBUFFERINGTHRESHOLD : Int = 10



  implicit val uniqueRLitCache = env.uniqueRLitCache
  implicit val uniqueLiteralResolver = env.uniqueLiteralResolver

  var state: ProvingState = STOPPED


  var keptClausesCount: Int = 0
  var derivedClausesCount: Int = 0

  var recievedClauseCount: Int = 0
  var recievedKeptClauseCount: Int = 0

  var dispatchedClauseCount: Int = 0


  var localAllocation: List[String] = Nil

  var recievedClausesCount: Int = 0


  // this values should ideally be adjusted during runtime
  val MESSAGEBUFFERPURGECLAUSECOUNTTHRESHOLD = 0


  val MESSAGEBUFFERPURGETIMETHRESHOLD = 0 // in milliseconds




  val messageBuffer: Queue[FOLClause] = new Queue[FOLClause]()


  private[this] def setState(newState: ProvingState) {
    log.debug("%s transitions from %s to %s state", this, state, newState)
    state = newState
    //     inform supervisor rsabout state change
    parent.get ! ProverStatus(state, keptClausesCount, derivedClausesCount, recievedKeptClauseCount, recievedClauseCount, dispatchedClauseCount)


  }


  /**
   * Callback from proving alogorithm
   *
   */
  def receiveUpdate(resolutionResult: Any) {
    //println("Recieved derived clauses , sending them to dispatcher actor")
    // send derived clauses to clause dispatcher
    parent match {
      case Some(parentActor) => {
        resolutionResult match {

          case GivenClause(givenClause) => {
            givenClause.uniqueResolvableLit match {
              case Some(urlit) if (localAllocation.contains(urlit.top)) => {
                // says here , ignore
              }

              case None => {
                // no unique lit ignore
              }

              case Some(urlit) => {
                // dispatch
                // pass the derived clauses to the parent actor
                log.warning("Given clause %s is not allocated to this reasoner, dispatch it",givenClause)
                dispatchedClauseCount += 1
                parentActor forward resolutionResult

              }

            }



          }


          case derived: Derived => {
            val derivedClause = derived.derived
            val urlit = derivedClause.uniqueResolvableLit

            urlit match {
              case Some(urlit) if (localAllocation.contains(urlit.top)) => {
                // says here , add to usable
                prover.addToUsable(derivedClause)
              }

              case None => {
                // no unique lit .. stays here
                // says here , add to usable
                prover.addToUsable(derivedClause)
              }

              case Some(urlit) => {
                // dispatch
                // pass the derived clauses to the parent actor
                dispatchedClauseCount += 1

                // add to local workedOff]

                prover.addToWorkedOff(derivedClause)

                parentActor ! (derived, this)

              }

            }

          }

          case DerivedBatch(derivedClauses) => {

            // split clauses into those that stay at this node, and those that get
            // sent away
            val localClauses = derivedClauses.filter({
              derivedClause: FOLClause =>
                      derivedClause.uniqueResolvableLit match {
                        case Some(urlit) if (localAllocation.contains(urlit.top)) => {
                          true
                        }

                        case None => {
                          // no unique lit .. stays here
                          true
                        }

                        case Some(urlit) => {
                          // dispatch
                          // pass the derived clauses to the parent actor
                          dispatchedClauseCount += 1
                          false
                        }
                      }
            })

            // first add those clauses staying here to usabel
            if (!localClauses.isEmpty)
              prover.addAllToUsable(localClauses)

            // send the rest to dispatcher ,and add to local workedoff


            val dispatchClauses = derivedClauses.toList -- localClauses.toList

            if (!dispatchClauses.isEmpty)
              prover.addAllToWorkedOff(dispatchClauses)

            if (!(dispatchClauses).isEmpty)
              parentActor ! DerivedBatch(dispatchClauses)


          }
          case _ => throw new IllegalArgumentException("Callback can only process ClauseStores")
        }
      }

      case None => throw new IllegalStateException("Proving Actor needs a parent actor")
    }


  }


  protected def receive = {
    // handle incoming allocation table updates
    case LocalSymbols(allocation) => {
      // update the allocation map
      localAllocation = allocation
      //      log.error("Hi iam reasoner with uuid : %s and my local symbols are :%s",this.uuid,localAllocation)


    }


    case GetEventLog(sessiontoken) => {
      eventRecorder match {
        case Some(recorder) => {
          reply(EventLog(recorder.events))
        }
        case None => {
          log.error("There was no event recorder, returning empty event log to %s",sender.toString)
          reply(EventLog(Nil))
        }
      }
    }


    case PurgeClauseBuffer => {
      if (messageBuffer.size > 0) {
        val clauses = messageBuffer.dequeueAll(_ => true)
        doSaturate(clauses)
      }
    }


    case msg@Saturate(clauses) => {
      log.debug("%s recieved clauses", this)
      // record those recievied clauses
      eventRecorder match {
        case Some(recorder) => {
          for (clause <- clauses) {
            recorder.recordRecievedClause(clause, sender.toString)
          }
        }

        case None => // no inference recorder present
      }




      // if current mailbox is over threshold size buffer the clasues
//      if(this.mailbox.size > MAILBOXBUFFERINGTHRESHOLD) {
//        log.info("Buffering incoming claueses")
//        incomingClausesBuffer.append(clauses)
//      }
//      else {
//
//        if(!incomingClausesBuffer.isEmpty) {
//          val buffered = incomingClausesBuffer.toList.flatten(i => i)
//          log.info("Purging incoming clause buffer , buffer size was : %s",incomingClausesBuffer.size)
//          // purge buffer + forward to prover
//          doSaturate(clauses ++ buffered)
//          // clear out the buffer
//          incomingClausesBuffer.clear
//        } else {
//          // directly forward to prover
//          doSaturate(clauses)
//
//        }
//      }

      // directly forward to prover
          doSaturate(clauses)

    }

    case msg@SaturateInitial(initialClauses) => {
      log.debug("%s recieved initial clauses , staring saturation", this)
      doSaturate(initialClauses)
    }


    case msg@GetStatus(bla) => {
      log.debug("Recieved Status Request Message")
      reply(Status("Status : %s" format (state)))


    }


    case GetKeptClauses(bla) => {
      log.debug("Recieved GetKeptClauses Request Message")
      reply(KeptClauses(prover.workedOff))
    }


    case msg@GetRecievedClausesCount(status) => {
      log.debug("%s Recieved RecievedClausesCount Message ", status)
      log.info("This reasoner has RECIEVED clauses count: %s", recievedClausesCount)
      reply(RecievedClausesCount(recievedClausesCount))
    }


  }


  private def doSaturate(clauses: Iterable[FOLClause]) = {
    setState(SATURATING);
    val result = prover.saturate(clauses)
    // finished saturatign

    result match {
      case (COMPLETION, inKeptClausesCount, inDerivedClausesCount, inRecievedClauseCount, inRecievedKeptClauseCount) => {
        // save the kept clauses
        keptClausesCount = inKeptClausesCount
        derivedClausesCount = inDerivedClausesCount
        recievedKeptClauseCount = inRecievedKeptClauseCount
        recievedClauseCount = inRecievedClauseCount

        log.info("LOCALY SATURATED ... ")
        // clear the initial clauses
        setState(SATISFIABLE)
      }
      case (PROOF, inKeptClausesCount, inDerivedClausesCount, inRecievedClauseCount, inRecievedKeptClauseCount) => {
        log.info("PROOF FOUND ! ")
        keptClausesCount = inKeptClausesCount
        derivedClausesCount = inDerivedClausesCount
        recievedKeptClauseCount = inRecievedKeptClauseCount
        recievedClauseCount = inRecievedClauseCount

        setState(UNSATISFIABLE)
      }

      case (TIMEUP, inKeptClausesCount, inDerivedClausesCount, inRecievedClauseCount, inRecievedKeptClauseCount) => {
        keptClausesCount = inKeptClausesCount
        derivedClausesCount = inDerivedClausesCount
        recievedKeptClauseCount = inRecievedKeptClauseCount
        recievedClauseCount = inRecievedClauseCount
        log.info("TIMELIMIT REACHED ! ")
        setState(SATISFIABLE)
      }

      case (ERROR, inKeptClausesCount, inDerivedClausesCount, inRecievedClauseCount, inRecievedKeptClauseCount) => {
        keptClausesCount = inKeptClausesCount
        derivedClausesCount = inDerivedClausesCount
        recievedKeptClauseCount = inRecievedKeptClauseCount
        recievedClauseCount = inRecievedClauseCount

        log.error("%s ERROR !! ", this)
        error("Error during reasoning")
      }

    }
  }


  override protected def init = {
//    log.info("Scheduling heartbeat")
    // dispatch messegate to ourself with a fixed interval , upon recieving we will check the status of subsctors and
    // take action if we are considered idle
    //    se.scalablesolutions.akka.actor.Scheduler.schedule(this, PurgeClauseBuffer, 0, MESSAGEBUFFERPURGETIMETHRESHOLD, TimeUnit.MILLISECONDS)

  }

  override def shutdown = {
    log.info("Core prooving subsystem is shutting down...")
  }

}






