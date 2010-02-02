package allocation


import core.containers.ClauseStorage
import reasoner.{Reasoning}
import se.scalablesolutions.akka.actor.Actor

/**
 * User: nowi
 * Date: 22.01.2010
 * Time: 15:09:44
 */

class NaiveOneToOneUnrestrictedLocalAllocator extends Allocating {
  override def allocate(modules: List[ClauseStorage], reasoners: List[Actor with Reasoning]): Map[ClauseStorage, Actor with Reasoning] = {
    // size has to match
    require(modules.size == reasoners.size)
    // create naive 1 to 1 mapping
    Map() ++ (modules zip reasoners)
  }

}