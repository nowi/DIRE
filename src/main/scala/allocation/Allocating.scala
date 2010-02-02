package allocation


import core.containers.ClauseStorage
import reasoner.{Reasoning}
import se.scalablesolutions.akka.actor.Actor

/**
 * Allocation takes care of mapping Sets of Clauses ( our KBs ) to distinct Reasoner nodes
 * Specific strategies can be applied taking into account parameters of the modulu and the
 * available resources at the reasoning node ( cpu, ram, latency vs. module size , connectedness
 * beetween modules) 
 * User: nowi
 * Date: 22.01.2010
 * Time: 14:08:13
 */

trait Allocating {
  def allocate(modules: List[ClauseStorage], reasoners: List[Actor with Reasoning]): Map[ClauseStorage, Actor]
}