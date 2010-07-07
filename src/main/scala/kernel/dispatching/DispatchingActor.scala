package de.unima.dire.kernel.dispatching

import de.unima.dire.kernel._
import de.unima.dire.core.containers.FOLClause

import collection.mutable.MultiMap
import se.scalablesolutions.akka.actor.{Actor, ActorRef}

trait ReasoningActorChild {
  var parent : Option[ActorRef] = None
}


trait DispatchingActor extends Actor with ReasoningActorChild {
  var allocationTable: Map[String, ActorRef] = Map()

  var dispatchedCount: Int = 0


  override def init = {
    log.debug("%s initialized", this)
  }

  protected def receive = {
    // handle incoming allocation table updates
    case LoadAllocation(allocation, localAddress) => {
      //      log.trace("Reasoner: %s Recieved Load Allocation Message forwared from %s with payload %s", this,forwarder, ((allocation map {case (sig, uuid) => ("Signature" + sig + "-->" + "Reasoner :" + uuid)}) mkString ("AllocationTable : [\n", ",\n", "]")))
      // update the allocation map
      allocationTable = allocation

    }

    // handle incoming parent actor registration
    case RegisterParentActor(parentActor) => {
      // if paretn is already defined fail
      if(parent.isDefined) throw new IllegalStateException("This actor has already a registered parent !")
      else parent = Some(parentActor)
    }


    case msg@GetDispatchedClausesCount(status) => {
      log.debug("%s Recieved GetDispatchedClausesCount Message ", status)
      // forward to dispatching actor
      reply(DispatchedClausesCount(dispatchedCount))
    }

    // handle dispatching of derived clauses
    case Derived(resolvedClause, _, _) => {

      // pass clause to logging actor
      // analyze where this claues should be send
      for ((destinationActor, clauses) <- determineDestination(List(resolvedClause), allocationTable)) {
        // disptach
        //println("%s is Dispatching CLauses : %s to destination kernel : %s",this,clauses,actor)
        clauses match {
          case cs if (!cs.isEmpty) => {
            log.debug("Dispatching clauses %s to reasoner %s", clauses, destinationActor)
            dispatchedCount = dispatchedCount + clauses.toList.size
            val clausepacket = cs
            val da = destinationActor
            destinationActor ! Saturate(clauses)

          }
          case _ => // dont sent
        }


      }


    }

    // handle dispatching of given clause
    case msg@GivenClause(givenClause) => {

      // pass clause to logging actor
      // analyze where this claues should be send
      for ((destinationActor, clauses) <- determineDestination(List(givenClause), allocationTable)) {
        // disptach
        //println("%s is Dispatching CLauses : %s to destination kernel : %s",this,clauses,actor)
        clauses match {
          case cs if (!cs.isEmpty) => {
            log.debug("Dispatching given clauses %s to reasoner %s", clauses, destinationActor)
            dispatchedCount = dispatchedCount + clauses.toList.size
            val clausepacket = cs
            val da = destinationActor
            destinationActor ! Saturate(clauses)

          }
          case _ => // dont sent
        }


      }


    }

    case DerivedBatch(resolvedClauses) => {

      // pass clause to logging actor
      // analyze where this claues should be send
      for ((destinationActor, clauses) <- determineDestination(resolvedClauses, allocationTable)) {
        // disptach
        //println("%s is Dispatching CLauses : %s to destination kernel : %s",this,clauses,actor)
        clauses match {
          case cs if (!cs.isEmpty) => {
            log.debug("Dispatching clauses %s to reasoner %s", clauses, destinationActor)
            dispatchedCount = dispatchedCount + clauses.toList.size
            //log.error("Sending clause packet of size %s",cs.size)
            destinationActor ! Saturate(clauses)
          }
          case _ => // dont sent
        }


      }


    }

    // handle additional message type dispatching here !



  }


  protected def determineDestination(clauses: Iterable[FOLClause], allocation: Map[String, ActorRef]): MultiMap[ActorRef, FOLClause]
}








