package kernel


import allocation.ClauseAllocation
import collection.mutable.MultiMap
import core.containers.{CNFClauseStore, ClauseStorage}
import core.resolution.SuccessfullResolution
import domain.fol.ast.FOLClause
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.dispatch.Dispatchers

trait ReasoningActorChild {
  var  parent : Option[ReasoningActor] = None

  def link(newParent : Option[ReasoningActor]) {
    require(!parent.isDefined && newParent.isDefined)
    parent = newParent
  }
}


trait DispatchingActor extends Actor with ReasoningActorChild  {
  var allocationTable: Map[String,Any] = Map()

  var dispatchedCount : Int = 0



  override protected def init = {
    log.debug("%s initialized",this)
  }

  protected def receive = {
    // handle incoming allocation table updates
    case LoadAllocation(allocation,localAddress) => {
//      log.trace("Reasoner: %s Recieved Load Allocation Message forwared from %s with payload %s", this,forwarder, ((allocation map {case (sig, uuid) => ("Signature" + sig + "-->" + "Reasoner :" + uuid)}) mkString ("AllocationTable : [\n", ",\n", "]")))
      // update the allocation map
      allocationTable = allocation

    }

    case msg @ GetDispatchedClausesCount(status) => {
      log.debug("%s Recieved GetDispatchedClausesCount Message ",status)
      // forward to dispatching actor
      reply(DispatchedClausesCount(dispatchedCount))
    }

    // handle dispatching of derived clauses
    case Derived(resolvedClause,_,_) => {

      // pass clause to logging actor
      // analyze where this claues should be send
      for((destinationActor,clauses) <- determineDestination(List(resolvedClause),allocationTable) ){
        // disptach
        //println("%s is Dispatching CLauses : %s to destination kernel : %s",this,clauses,actor)
        clauses match {
          case cs if(!cs.isEmpty) => {
            log.debug("Dispatching clauses %s to reasoner %s", clauses,destinationActor)
            dispatchedCount = dispatchedCount + clauses.toList.size
            val clausepacket = cs
            val da = destinationActor
            destinationActor.send(Saturate(clauses))

          }
          case _ => // dont sent
        }


      }


    }

    // handle dispatching of given clause
    case  msg @ GivenClause(givenClause) => {

      // pass clause to logging actor
      // analyze where this claues should be send
      for((destinationActor,clauses) <- determineDestination(List(givenClause),allocationTable) ){
        // disptach
        //println("%s is Dispatching CLauses : %s to destination kernel : %s",this,clauses,actor)
        clauses match {
          case cs if(!cs.isEmpty) => {
            log.debug("Dispatching given clauses %s to reasoner %s", clauses,destinationActor)
            dispatchedCount = dispatchedCount + clauses.toList.size
            val clausepacket = cs
            val da = destinationActor
            destinationActor.send(Saturate(clauses))

          }
          case _ => // dont sent
        }


      }


    }

    case DerivedBatch(resolvedClauses) => {

      // pass clause to logging actor
      // analyze where this claues should be send
      for((destinationActor,clauses) <- determineDestination(resolvedClauses,allocationTable) ){
        // disptach
        //println("%s is Dispatching CLauses : %s to destination kernel : %s",this,clauses,actor)
        clauses match {
          case cs if(!cs.isEmpty) => {
            log.debug("Dispatching clauses %s to reasoner %s", clauses,destinationActor)
            dispatchedCount = dispatchedCount + clauses.toList.size
            //log.error("Sending clause packet of size %s",cs.size)
            destinationActor.send(Saturate(clauses))
          }
          case _ => // dont sent
        }


      }


    }

    // handle additional message type dispatching here !



  }



  protected def determineDestination(clauses: Iterable[FOLClause], allocation: Map[String, Any] ) : MultiMap[Actor, FOLClause]
}

/**
 * Broadcasts clauses to all reasoners known in our allocation table , this dispatcher is just
 * an example and not really usefull
 */











