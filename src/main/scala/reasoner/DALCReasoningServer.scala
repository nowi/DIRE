package reasoner


import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.config.OneForOneStrategy

/**
 * The Main reasoning actor
 * User: nowi
 * Date: 22.01.2010
 * Time: 15:43:22
 */

trait DALCReasoningServer extends Actor {
  // configuration of core reasoning components

  faultHandler = Some(OneForOneStrategy(5, 5000))
  trapExit = List(classOf[Exception])

  log.info("DALCReasoningServer is starting up...")

  val config = new Object {
  }

  // the first order logic proover useed
  //val proover: FOLProving = core.ResolutionProover1(config)

  // actor message handler
  def receive = reasoningManagement orElse bootstrappingManagement


  // abstract methods to be defined somewhere else
  protected def reasoningManagement: PartialFunction[Any, Unit]

  // abstract methods to be defined somewhere else
  protected def bootstrappingManagement: PartialFunction[Any, Unit]




  //override def init = startLink(storage)

  override def shutdown = {
    log.info("Chat server is shutting down...")
  }
  // outbox actor

  // inbox actor



}

object DALCReasoningServer {
}