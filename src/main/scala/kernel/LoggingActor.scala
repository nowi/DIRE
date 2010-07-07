package de.unima.dire.kernel


import de.unima.dire.kernel.dispatching.ReasoningActorChild
import de.unima.dire.core.resolution.SuccessfullResolution
import de.unima.dire.recording.Neo4JRecorder
import de.unima.dire.core.containers.FOLClause

import se.scalablesolutions.akka.actor.Actor
/**
 * User: nowi
 * Date: 25.03.2010
 * Time: 14:59:25
 */

trait LoggingActor extends Actor with ReasoningActorChild {
  override def init = {
    log.debug("%s initialized", this)
  }

  protected def receive = {
    // handle initial clause
//    case msg @ (LoadClauses(clauses), sender: Actor) => {
//      // log derived clause
//      log.debug(this + " logging clauses")
//      for(clause <- clauses)
//        recordClause(clause,None,None)
//
//
//
//    }

    // handle derived clauses
    case msg@(Derived(clause,None,None), sender: Actor) => {
      // cannot log clause without parents log derived clause
      log.debug(this + "cannot log clause without parents log derived clause")
      //recordClause(clause,Some(parent1),Some(parent2))
    }

    // handle derived clauses
    case msg@(Derived(clause,parent1,parent2), sender: Actor) => {
      // log derived clause
      log.debug(this + " logging clauses")
    recordClause(clause,parent1,parent2)
  }


  }

  protected def recordClause(clause: FOLClause, parent1: Option[FOLClause], parent2: Option[FOLClause])





}




class Neo4JLoggingActor(env: {val neo4JGraphBasePath : String}) extends LoggingActor {

  val neo4JGraphBasePath : String = env.neo4JGraphBasePath

  // init logger instance
  val neo4jLogger = new Neo4JRecorder(neo4JGraphBasePath + "/"+ System.currentTimeMillis +"/" + this)



  override def recordClause(clause: FOLClause, parent1: Option[FOLClause], parent2: Option[FOLClause]) {
    neo4jLogger.recordClause(clause,parent1,parent2)

  }




}