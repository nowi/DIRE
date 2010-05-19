package kernel


import collection.mutable.ArrayBuffer
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.Date
import se.scalablesolutions.akka.dispatch.Dispatchers
import core._
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
import resolution.{SuccessfullResolution}
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.util.Logging
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 03.02.2010
 * Time: 17:11:44
 */

abstract class ProvingActor extends Actor with ReasoningActorChild {


  // cofigure a native os thread based dispatcher for the proving actor
    val d = Dispatchers.newThreadBasedDispatcher(this)
//    val d = Dispatchers.globalReactorBasedSingleThreadEventDrivenDispatcher
//    val d = Dispatchers.newExecutorBasedEventDrivenDispatcher("name");
//  d.withNewThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity
//    .setCorePoolSize(16)
//    .setMaxPoolSize(128)
//    .setKeepAliveTimeInMillis(60000)
//    .setRejectionPolicy(new CallerRunsPolicy)
//    .buildThreadPool;





    messageDispatcher = d

  val prover: FOLProving


  var state: ProvingState = STOPPED

  var initialClauses: Iterable[FOLClause] = Nil
  var keptClauses: Iterable[FOLClause] = Nil


  var recievedClausesCount: Int = 0


  private[this] def setState(newState: ProvingState) {
    log.info("%s transitions from %s to %s state", this, state, newState)
    state = newState
//     inform supervisor rsabout state change
    parent.get ! ProverStatus(state,keptClauses.toList.size,0)


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
            // pass the derived clauses to the parent actor
            parentActor ! (derived, this)

          }
          case _ => throw new IllegalArgumentException("Callback can only process ClauseStores")
        }
      }

      case None => throw new IllegalStateException("Proving Actor needs a parent actor")
    }


  }


  protected def receive = {
    case msg @ Saturate(clauses) => {

      // the kb is satisfiable, we can resume prooving with the keptclauses , and add the recieved clause
      recievedClausesCount = recievedClausesCount + clauses.toList.size

      setState(SATURATING);
      val result = prover.saturate(clauses)
      // finished saturatign

      result match {
        case (COMPLETION, clauses) => {
          // save the kept clauses
          keptClauses = clauses

          log.info("LOCALY SATISFIED ... ")
          // clear the initial clauses
          setState(SATISFIABLE)
        }
        case (PROOF, clauses) => {
          log.info("LOCALY UNSATISFIED ! ")
          setState(UNSATISFIABLE)
        }

        case (TIMEUP, clauses) => {
          log.info("TIMELIMIT REACHED ! ")
          setState(SATISFIABLE)
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






