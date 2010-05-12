package allocation


import core.containers.ClauseStorage
import helpers.Logging
import se.scalablesolutions.akka.actor.Actor

/**
 * User: nowi
 * Date: 22.01.2010
 * Time: 15:09:44
 */

class NaiveOneToOneUnrestrictedLocalAllocator extends Allocating with Logging {
  override def allocate(modules: List[ClauseStorage], reasoners: List[Actor]): Map[ClauseStorage, Actor] = {
    log.info("%s is allocating modules %s to reasoners %s", this, modules, reasoners)
    // size has to match
    //require(modules.size == reasoners.size)
    // create naive 1 to 1 mapping
    Map() ++ (modules zip reasoners)
  }

}