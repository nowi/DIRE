package kernel

import collection.mutable.ArrayBuffer
import core.containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.FOLClause
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.Date
import ProvingState._
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.config.OneForOneStrategy
import se.scalablesolutions.akka.dispatch.Dispatchers

/**
 * The Main reasoning actor
 * User: nowi
 * Date: 22.01.2010
 * Time: 15:43:22
 */

abstract class ReasoningActor extends Actor {
  // configuration of core reasoning components

  id = this.uuid

  faultHandler = Some(OneForOneStrategy(5, 5000))
  trapExit = List(classOf[Exception])


  var manager : Option[Actor] = None


  // child prooving actor
  val provingActor: ProvingActor
  // child dispatcher  actor
  val dispatcherActor: DispatchingActor

  // child derivations logging  actor
  val derivationsLoggingActor: LoggingActor

  // child reductions logging  actor
  val reductionsLoggingActor: LoggingActor





  //var proverStatus: ProvingState = STOPPED


  // abstract methods to be defined somewhere else
  protected def receive = {


    case msg@LoadAllocation(allocation) => {


      // forward to dispatcher
      dispatcherActor forward (msg, this)
    }


    case msg@GetStatus(bla) => {
      log.info("Recieved Status Request Message ,ignoring for now")
      //      proverStatus match {
      //        case LOADED | SATISFIABLE | UNSATISFIABLE=> provingActor forward msg
      //        case _ => reply(Status("WORKING ! , Core proving subsystem status = " + proverStatus))
      //      }


    }

    case msg@GetStatusOverride(bla) => {
      log.info("Recieved Status Overide Request Message.. forwarding to prover")
      provingActor forward GetStatus(bla)


    }


    case msg@GetKeptClauses(bla) => {
      log.trace("Recieved GetKeptClauses Request Message")

      provingActor forward msg

    }

    case msg@GetIncomingClausesLog(bla) => {
      log.trace("Recieved GetKeptClauses Request Message, ignoring")

      //    reply(incomingClausesLog)
      //
    }



    // INBOUND
    case msg @ Saturate(clauses) => {
      
      // the first time we recieve this message record the sender
      // we will report back to this sender in the future
      manager match {
        case None => {
          log.warning("%s recieved initial clauses from manager %s",this,sender.get)
          manager = sender
        }
        case Some(_) => // we have a manager actor stored
      }

      // forward to prover
      provingActor forward msg
    }

    // OUTBOUND
    // handle dispatching of derived clauses
    case msg@(derived: Derived, sender: Actor) => {
      log.trace("Recieved Derived Message with derived clauses %s..from sender : %s forwarding to %s ", derived, sender, dispatcherActor)
      // forward to logger
      //derivationsLoggingActor forward derived
      // forward to dispatcher
      // replace sender
      dispatcherActor ! (derived, this)
    }



    // communiucation with prover kernel , prover kernel tells its status
    case msg@ProverStatus(status,workedOffCount,derivedCount) => {
      log.info("Recieved ProverStatus Update Message.. new status of proover %s is %s", provingActor, status)

      // forward this to the manager actor

      manager match {
        case Some(manager) => manager ! msg
        case None => // no manager , cannot notify about progress
      }



    }


    case msg @ GetRecievedClausesCount(status) => {
      log.debug("%s Recieved GetDispatchedClausesCount Message ", status)
      // forward to proving actor
      provingActor forward msg
    }

    case msg@GetDispatchedClausesCount(status) => {
      log.debug("%s Recieved GetDispatchedClausesCount Message ", status)

      // forward to dispatching actor
      dispatcherActor forward msg
    }


  }


  override def init = {
    log.info("DALCReasoning actor is starting up and .. starting and linking %s and %s ", dispatcherActor, provingActor)
    dispatcherActor.start
    provingActor.start
    derivationsLoggingActor.start
    reductionsLoggingActor.start

    startLink(derivationsLoggingActor)
    startLink(reductionsLoggingActor)
    startLink(dispatcherActor)
    dispatcherActor.link(Some(this))
    startLink(provingActor)
    provingActor.link(Some(this))
  }

  override def shutdown = {
    log.info("DALCReasoningNode server is shutting down...")
    unlink(derivationsLoggingActor)
    unlink(dispatcherActor)
    unlink(provingActor)
  }
  // outbox actor

  // inbox actor



}

