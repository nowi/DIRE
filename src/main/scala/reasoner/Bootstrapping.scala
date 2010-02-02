package reasoner

/**
 * User: nowi
 * Date: 02.02.2010
 * Time: 15:59:45
 */


import se.scalablesolutions.akka.actor.Actor

/**
 * Implements reasoning.
 * <p/>
 * Uses self-type annotation 'this: Actor =>'
 * to declare that it needs to be mixed in with an Actor.
 */
trait Bootstrapping {
  this: Actor =>
  protected def bootstrappingManagement: PartialFunction[Any, Unit] = {
    case LoadAllocation(allocation) => {
      log.info("Reasoner: %s Recieved Load Allocation Message with payload %s", this, ((allocation map {case (sig, uuid) => ("Signature" + sig + "-->" + "Reasoner :" + uuid)}) mkString ("AllocationTable : [\n", ",\n", "]")))

    }

  }


}
