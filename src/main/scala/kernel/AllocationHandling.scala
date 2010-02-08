package kernel

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
trait AllocationHandling {
  this: Actor =>

  // reference to proover


 



}
