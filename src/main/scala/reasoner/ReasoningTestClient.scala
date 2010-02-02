package reasoner

import se.scalablesolutions.akka.actor.Actor

/**
 * User: nowi
 * Date: 22.01.2010
 * Time: 18:37:41
 */
class ReasoningTestClient(val name: String, reasoners: List[Actor]) {
  def startSatisfy {
    reasoners.foreach {_ ! StartSatisfy("blabla")}
  }

  def stopSatisfy {
    reasoners.foreach {_ ! StopSatisfy("blabla")}
  }
  //  def entails =                 reasoner.DALCReasoningServer ! Entail(StandardClause(domain.fol.ast.Predicate("Hi",Variable("migo"))))

}



