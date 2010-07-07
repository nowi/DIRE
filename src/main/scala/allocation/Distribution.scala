package de.unima.dire.allocation


import de.unima.dire.core.containers.ClauseStorage
import de.unima.dire.helpers.Logging

import se.scalablesolutions.akka.actor.ActorRef
import collection.MapProxy
/**
 * Allocation takes care of mapping Sets of Clauses ( our KBs ) to distinct Reasoner nodes
 * Specific strategies can be applied taking into account parameters of the modulu and the
 * available resources at the reasoning node ( cpu, ram, latency vs. module size , connectedness
 * beetween modules) 
 * User: nowi
 * Date: 22.01.2010
 * Time: 14:08:13
 */
trait Distribution {

  def distribute(modules: List[ClauseStorage], reasoners: List[ActorRef]) : Map[ActorRef, ClauseStorage]

}


class NaiveOneToOneUnrestrictedLocalDistributor extends Distribution with Logging{
  override def distribute(modules: List[ClauseStorage], reasoners: List[ActorRef]): Map[ActorRef, ClauseStorage] = {
    log.info("%s is distributing modules %s to reasoners %s", this, modules, reasoners)
    // size has to match
    //require(modules.size == reasoners.size)
    // create naive 1 to 1 mapping
    Map() ++ (reasoners zip modules)
  }

}


