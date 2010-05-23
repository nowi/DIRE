package kernel


import collection.mutable.ArrayBuffer
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.Date
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
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.util.Logging
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 03.02.2010
 * Time: 17:11:44
 */

class ProvingActor(env: {val prover: FOLProving; val uniqueLiteralResolver: UniqueLiteralResolution;val uniqueRLitCache: URLitCache}) extends Actor with ReasoningActorChild {


  // cofigure a native os thread based dispatcher for the proving actor
  val d = Dispatchers.newThreadBasedDispatcher(this)
  //    val d = Dispatchers.globalReactorBasedSingleThreadEventDrivenDispatcher
//      val d = Dispatchers.newExecutorBasedEventDrivenDispatcher("prover");
//    d.withNewThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity
//      .setCorePoolSize(5)
//      .setMaxPoolSize(128)
//      .setKeepAliveTimeInMillis(10000)
//      .setRejectionPolicy(new CallerRunsPolicy)
//      .buildThreadPool;

  messageDispatcher = d

  val prover: FOLProving = env.prover

  prover.addObserver(this)



  implicit val uniqueRLitCache  = env.uniqueRLitCache
  implicit val uniqueLiteralResolver  = env.uniqueLiteralResolver

  var state: ProvingState = STOPPED


  var keptClausesCount : Int = 0
  var derivedClausesCount : Int = 0

  var recievedClauseCount: Int = 0
  var recievedKeptClauseCount: Int = 0

  var dispatchedClauseCount :Int = 0


  var localAllocation: List[String] = Nil

  var recievedClausesCount: Int = 0


  private[this] def setState(newState: ProvingState) {
    log.info("%s transitions from %s to %s state", this, state, newState)
    state = newState
    //     inform supervisor rsabout state change
    parent.get ! ProverStatus(state, keptClausesCount, derivedClausesCount,recievedKeptClauseCount,recievedClauseCount,dispatchedClauseCount)


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
          case derived: Derived => {
            val derivedClause = derived.derived
            val urlit = derivedClause.uniqueResolvableLit

            urlit match {
              case Some(urlit) if(localAllocation.contains(urlit.top)) => {
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
            val localClauses = derivedClauses.filter({derivedClause : FOLClause =>
                    derivedClause.uniqueResolvableLit match {
                      case Some(urlit) if(localAllocation.contains(urlit.top)) => {
                        // says here , add to usable
                        true
                      }

                      case None => {
                        // no unique lit .. stays here
                        // says here , add to usable
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
            if(!localClauses.isEmpty)
              prover.addAllToUsable(localClauses)

            // send the rest to dispatcher ,and add to local workedoff


            val dispatchClauses = derivedClauses.toList -- localClauses.toList

            if(!dispatchClauses.isEmpty)
              prover.addAllToWorkedOff(dispatchClauses)

            if(!(dispatchClauses).isEmpty)
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


    case msg@Saturate(clauses) => {
//      log.error("Hi iam reasoner with uuid : %s and i start staruation on clauses :%s",this.uuid,clauses)
      // the kb is satisfiable, we can resume prooving with the keptclauses , and add the recieved clause
      recievedClausesCount = recievedClausesCount + clauses.toList.size

      setState(SATURATING);
      val result = prover.saturate(clauses)
      // finished saturatign

      result match {
        case (COMPLETION, inKeptClausesCount,inDerivedClausesCount,inRecievedClauseCount,inRecievedKeptClauseCount) => {
          // save the kept clauses
          keptClausesCount = inKeptClausesCount
          derivedClausesCount = inDerivedClausesCount
          recievedKeptClauseCount = inRecievedKeptClauseCount
          recievedClauseCount = inRecievedClauseCount

          log.info("LOCALY SATISFIED ... ")
          // clear the initial clauses
          setState(SATISFIABLE)
        }
        case (PROOF, inKeptClausesCount,inDerivedClausesCount,inRecievedClauseCount,inRecievedKeptClauseCount) => {
          log.info("LOCALY UNSATISFIED ! ")
          keptClausesCount = inKeptClausesCount
          derivedClausesCount = inDerivedClausesCount
          recievedKeptClauseCount = inRecievedKeptClauseCount
          recievedClauseCount = inRecievedClauseCount

          setState(UNSATISFIABLE)
        }

        case (TIMEUP, inKeptClausesCount,inDerivedClausesCount,inRecievedClauseCount,inRecievedKeptClauseCount) => {
          keptClausesCount = inKeptClausesCount
          derivedClausesCount = inDerivedClausesCount
          recievedKeptClauseCount = inRecievedKeptClauseCount
          recievedClauseCount = inRecievedClauseCount
          log.info("TIMELIMIT REACHED ! ")
          setState(SATISFIABLE)
        }

        case (ERROR, inKeptClausesCount,inDerivedClausesCount,inRecievedClauseCount,inRecievedKeptClauseCount) => {
          keptClausesCount = inKeptClausesCount
          derivedClausesCount = inDerivedClausesCount
          recievedKeptClauseCount = inRecievedKeptClauseCount
          recievedClauseCount = inRecievedClauseCount

          log.error("%s ERROR !! ",this)
          error("Error during reasoning")
        }

      }


    }


    case msg@GetStatus(bla) => {
      log.debug("Recieved Status Request Message")
      reply(Status("Status : %s" format (state)))


    }


    case GetKeptClauses(bla) => {
      log.info("Recieved GetKeptClauses Request Message")

      //      state match {
      //        case SATISFIABLE => reply(KeptClauses(keptClauses))
      //        case _ => {
      //          log.warning("KeptCLauses have been requested but prover is not in state SATISFIABLE")
      //          reply(KeptClauses(keptClauses))
      //        }
      //      }

    }


    case msg@GetRecievedClausesCount(status) => {
      log.debug("%s Recieved RecievedClausesCount Message ", status)
      log.info("This reasoner has RECIEVED clauses count: %s", recievedClausesCount)
      reply(RecievedClausesCount(recievedClausesCount))
    }


  }

  override def shutdown = {
    log.info("Core prooving subsystem is shutting down...")
  }

}






