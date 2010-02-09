package kernel


import core.containers.{CNFClauseStore, NonEmptyClauseStore, ClauseStorage}
import domain.fol.ast.FOLClause
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.dispatch.Dispatchers


trait DispatchingActor extends Actor  {
  // reference to directory service actor
  var allocationTable: Map[Set[String], String] = Map()


  override protected def init = {
    log.debug("%s initialized",this)
  }

  protected def receive = {
    // handle incoming allocation table updates
    case (LoadAllocation(allocation),forwarder : Actor) => {
      log.trace("Reasoner: %s Recieved Load Allocation Message forwared from %s with payload %s", this,forwarder, ((allocation map {case (sig, uuid) => ("Signature" + sig + "-->" + "Reasoner :" + uuid)}) mkString ("AllocationTable : [\n", ",\n", "]")))
      // update the allocation map
      allocationTable ++= allocation

    }

    // handle dispatching of derived clauses
    case (Entail(allClauses),forwarder : Actor) => {
      log.trace("Reasoner: %s Recieved OutboundClauses Message , forwarded by %s with payload %s", this,forwarder, allClauses)

      // analyze where this claues should be send
      for((actor,clauses) <- determineDestination(allClauses,allocationTable) ){
        // disptach
        //println("%s is Dispatching CLauses : %s to destination kernel : %s",this,clauses,actor)
        clauses match {
          case NonEmptyClauseStore(cs) => actor ! Entail(clauses)
          case _ => // dont sent
        }


      }


    }

    // handle additional message type dispatching here !



  }



  protected def determineDestination(clauses: ClauseStorage, allocation: Map[Set[String], String]) : Map[Actor,ClauseStorage]
}

/**
 * Broadcasts clauses to all reasoners known in our allocation table , this dispatcher is just
 * an example and not really usefull
 */











