package kernel

import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
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

class DistributedALCReasoner(env: {
  val dispatcherActor : DispatchingActor;
  val provingActor : ProvingActor}) extends Actor {
  // configuration of core reasoning components

  id = this.uuid

  faultHandler = Some(OneForOneStrategy(5, 5000))
  trapExit = List(classOf[Exception])

  


   // child prooving actor
  val provingActor : ProvingActor = env.provingActor
  // child dispatcher  actor
  val dispatcherActor : DispatchingActor  = env.dispatcherActor

  var proverStatus: ProvingState = STOPPED







  // abstract methods to be defined somewhere else
  protected def receive = {
     case msg @ LoadClauses(clauses) => {
      // send those clauses to the
      provingActor ! (msg,this)
    }



 case msg @ LoadAllocation(allocation) => {
      // forward to dispatcher

      dispatcherActor forward (msg,this)
    }


    case msg @ GetStatus(bla) => {
      log.info("Recieved Status Request Message")
      proverStatus match {
        case LOADED | SATISFIABLE | UNSATISFIABLE=> provingActor forward msg
        case _ => reply(Status("WORKING ! , Core proving subsystem status = " + proverStatus)) 
      }


    }


  case msg @ GetKeptClauses(bla) => {
    log.trace("Recieved GetKeptClauses Request Message")

    provingActor forward msg

    }



    case msg @ Entail(clauses) => {
      log.trace("Recieved Entail Message with clauses to entail  %s.. forwarding to %s ",clauses, provingActor)

      provingActor forward (msg,this)

    }

    case msg @ StartSatisfy(message) => {
      // check state of prover
      proverStatus match {
        case LOADED | SATISFIABLE | UNSATISFIABLE => {
           log.trace("Recieved StartSatisfy Message.. forwarding to %s" , provingActor)
           provingActor ! (msg,this)
        }
        case _ => {
           log.trace("Recieved StartSatisfy Message.. forwarding to %s" , provingActor)
          provingActor ! (msg,this)
        }
      }

    }

    case msg @ StopSatisfy(message) => {
      log.debug("Recieved StopStatisfy Message .. forwarding to %s", provingActor)
      // forward to proover
      provingActor ! (msg,this)

    }


    // communiucation with prover kernel , prover kernel tells its status
    case msg @ ProverStatus(status) => {
      log.debug("Recieved ProverStatus Update Message.. new status of proover %s is %s", provingActor, status)
      log.debug("Recieved ProverStatus Update Message.. new status of proover ")
      // update the status of the prover
      proverStatus = status
    }
  }





  override def init = {
    log.info("DALCReasoning actor is starting up and .. starting and linking %s and %s ",dispatcherActor,provingActor)
      dispatcherActor.start
      provingActor.start

    startLink(dispatcherActor)
    startLink(provingActor)
  }

  override def shutdown = {
    log.info("DALCReasoningNode server is shutting down...")
    unlink(dispatcherActor)
    unlink(provingActor)
  }
  // outbox actor

  // inbox actor



}

