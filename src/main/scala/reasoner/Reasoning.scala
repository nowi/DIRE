package reasoner


import se.scalablesolutions.akka.actor.Actor

/**
 * Implements reasoning.
 * <p/>
 * Uses self-type annotation 'this: Actor =>'
 * to declare that it needs to be mixed in with an Actor.
 */
trait Reasoning {
  this: Actor =>
  protected def reasoningManagement: PartialFunction[Any, Unit] = {
    case Entail(clause) =>
      log.info("Recieved Entail Message with clause to entail  %s", clause)

    case StartSatisfy(message) =>
      log.info("Recieved StartSatisfy Message")

    case StopSatisfy(message) =>
      log.info("Recieved StopStatisfy Message")

    case LoadClauses(clauses) =>
      log.info("Reasoner: %s Recieved LoadClauses Message with payload %s", this, clauses)

  }


}
