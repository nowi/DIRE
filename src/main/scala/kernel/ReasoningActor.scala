package kernel

import collection.mutable.{ListBuffer, ArrayBuffer}
import core.containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.FOLClause
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent.TimeUnit
import java.util.Date
import java.util.logging.Logger
import ProvingState._
import se.scalablesolutions.akka.actor.{Scheduler, Actor}
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

  //  faultHandler = Some(OneForOneStrategy(5, 5000))
  faultHandler = None
  //  trapExit = List(classOf[Exception])
  trapExit = Nil

  

  var manager: Option[Actor] = None

  var recievedMessagesSinceLastHeartbeat: Boolean = false

  // child prooving actor
  val provingActor: ProvingActor
  // child dispatcher  actor
  val dispatcherActor: DispatchingActor

  // child derivations logging  actor
  val derivationsLoggingActor: LoggingActor

  // child reductions logging  actor
  val reductionsLoggingActor: LoggingActor


  var proverStatus: ProvingState = STOPPED
  var workedOffCount: Int = 0
  var derivedCount: Int = 0
  var recievedKeptClauseCount: Int = 0
  var recievedClauseCount: Int = 0
  var dispatchedClauseCount: Int = 0



  //var proverStatus: ProvingState = STOPPED


  // abstract methods to be defined somewhere else
  protected def receive = {

    case Shutdown(bla : String) => {
      log.error("Recived shutdown message")
      stop
    }


    case msg@LoadAllocation(allocation, localAddress) => {
      recievedMessagesSinceLastHeartbeat = true

      // forward to dispatcher
      dispatcherActor forward msg


      // forward to prover

      // prover only has a map of top symbols that this reasoning node is responsible
      // for , clauses for which he has no entry he passes on to dispatcher

      // filter out all symbols that have this reasoner as entry
      val localSymbols = new ListBuffer[String]()
      allocation.foreach {
        case (symbol, address) => {
          if (address.equals(localAddress)) {

            localSymbols += symbol
          }
        }
      }



      provingActor ! LocalSymbols(localSymbols.toList)


    }

    case msg @ GetEventLog(sessionToken) => {
      provingActor forward msg
    }

    case msg@GetStatus(bla) => {
      reply(ProverStatus(proverStatus, workedOffCount, derivedCount, recievedKeptClauseCount, recievedClauseCount, dispatchedClauseCount))


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
    case msg@Saturate(clauses) => {

      provingActor ! msg

    }

    // INBOUND initial
    case msg @ SaturateInitial(clauses) => {
      // the first time we recieve this message record the sender
      // we will report back to this sender in the future
      manager match {
        case None => {
          log.warning("%s recieved initial clauses from manager %s", this, sender.getOrElse("unknown"))
          manager = sender
        }
        case Some(_) => // we have a manager actor stored
      }

      // forward to prover
      provingActor ! msg
    }

    // OUTBOUND
    // handle dispatching of derived clauses
    case msg@Derived(_, _, _) => {
      //      log.trace("Recieved Derived Message with derived clauses %s..from sender : %s forwarding to %s ", derived, sender, dispatcherActor)
      // forward to logger
      //derivationsLoggingActor forward derived
      // forward to dispatcher
      // replace sender
      dispatcherActor ! msg
    }


    // OUTBOUND
    // handle given clause that is allocated to another reasoner , this should ideally
    // be done prior to starting stauration
    case msg@GivenClause(_) => {
      //      log.trace("Recieved Derived Message with derived clauses %s..from sender : %s forwarding to %s ", derived, sender, dispatcherActor)
      // forward to logger
      //derivationsLoggingActor forward derived
      // forward to dispatcher
      // replace sender
      dispatcherActor ! msg
    }


    // OUTBOUND
    // handle dispatching of derived cla
    // uses
    case msg@DerivedBatch(_) => {
      // TODO atm derived batch does not carry the parent clause informationn
      // so keept his in mind when forwardnig to logging actors
      // forward to logger
      //derivationsLoggingActor forward derived
      // forward to dispatcher
      // replace sender
      dispatcherActor ! msg
    }



    // communiucation with prover kernel , prover kernel tells its status
    case msg@ProverStatus(status, inWorkedOffCount, inDerivedCount, inRecievedKeptClauseCount, inRecievedClauseCount, inDispatchedClauseCount) => {
      log.debug("Recieved ProverStatus Update Message.. new status of proover %s is %s", provingActor, status)
      recievedMessagesSinceLastHeartbeat = false

      //proverStatus = status
      workedOffCount = inWorkedOffCount
      derivedCount = inDerivedCount
      recievedKeptClauseCount = inRecievedKeptClauseCount
      recievedClauseCount = inRecievedClauseCount
      dispatchedClauseCount = inDispatchedClauseCount

    }


    case msg@GetRecievedClausesCount(status) => {
      log.debug("%s Recieved GetDispatchedClausesCount Message ", status)
      // forward to proving actor
      provingActor forward msg
    }

    case msg@GetDispatchedClausesCount(status) => {
      log.debug("%s Recieved GetDispatchedClausesCount Message ", status)

      // forward to dispatching actor
      dispatcherActor forward msg
    }


    case msg@Heartbeat(string) => {
//      log.fatal("%s Recieved Heartbeat Message ", this)
//      log.fatal("%s | Kept : %s | DerivedCount : %s | Rec : %s | RecKept : %s | Dspt : %s", this, workedOffCount, derivedCount, recievedClauseCount, recievedKeptClauseCount, dispatchedClauseCount)
//      log.fatal("%s | Mailbox sizes \n", this)
//      log.fatal("%s | Prover : %s", this, provingActor.mailbox.size)
//      log.fatal("%s | Dispatcher : %s", this, dispatcherActor.mailbox.size)


//      log.fatal("%s%10d%10d%10d%10d%10d%10d%10d|", this, workedOffCount, derivedCount, recievedClauseCount, recievedKeptClauseCount, dispatchedClauseCount,provingActor.mailbox.size,dispatcherActor.mailbox.size)
      log.fatal("%s%10d%10d%10d%10d%10d|", this, workedOffCount, derivedCount, recievedClauseCount, recievedKeptClauseCount, dispatchedClauseCount)

//      log.fatal("%s |Kept|Derived|Recieved|RecKept|Sent|Inbox|Outbox| %s  %s  s%  %s  %s  %s  %s", this, workedOffCount, derivedCount, recievedClauseCount, recievedKeptClauseCount, dispatchedClauseCount,provingActor.mailbox.size,dispatcherActor.mailbox.size)
      //println("%s %s  %s  s%  %s  %s " format this, workedOffCount, derivedCount, recievedClauseCount, recievedKeptClauseCount, dispatchedClauseCount)

      //log.fatal("%s | Dispatcher : %s", this, dispatcherActor.mailbox.size)

      //log.error("%s Subsystems : prover is running %s | dispatcher is running: %s |", this, provingActor.isRunning, dispatcherActor.isRunning)
      // forward this to the manager actor
      //      manager match {
      //        case Some(manager) => {
      //          manager ! ProverStatus(proverStatus, workedOffCount, derivedCount, recievedKeptClauseCount, recievedClauseCount, dispatchedClauseCount)
      //        }
      //        case None => // no manager , cannot notify about progress
      //      }

    }


  }

  private def isIdle = {
    !recievedMessagesSinceLastHeartbeat
  }


  override def init = {
    log.error("DALCReasoning actor is starting up and .. starting and linking %s and %s ", dispatcherActor, provingActor)
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



    log.info("Scheduling heartbeat")
    // dispatch messegate to ourself with a fixed interval , upon recieving we will check the status of subsctors and
    // take action if we are considered idle
    Scheduler.schedule(this, Heartbeat("asjd"), 10, 5, TimeUnit.SECONDS)


  }

  override def shutdown = {
    log.error("DALCReasoningNode server is shutting down...")
    unlink(derivationsLoggingActor)
    unlink(dispatcherActor)
    unlink(provingActor)
    dispatcherActor.stop
    provingActor.stop
    derivationsLoggingActor.stop
    reductionsLoggingActor.stop
    Scheduler.shutdown




  }
  // outbox actor

  // inbox actor



}

