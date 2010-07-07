package de.unima.dire.kernel.dispatching

import de.unima.dire.core.containers.FOLClause

import collection.mutable._
import collection.mutable.{MultiMap, Set => MSet}
import se.scalablesolutions.akka.actor.{ActorRef, Actor}
import java.lang.String
import collection.immutable.Map
import collection.Iterable

/**
 * User: nowi
 * Date: 09.02.2010
 * Time: 10:35:14
 */

/**
 * Broadcasts clauses to into nirvana
 */
class ToVoidDispatchingActor extends DispatchingActor {
  override def determineDestination(clauses: Iterable[FOLClause], allocation: Map[String, ActorRef]): MultiMap[ActorRef, FOLClause] = {
    // create multimap actor --> List(clauses)
    val mapping: scala.collection.mutable.MultiMap[ActorRef, FOLClause] = new HashMap[ActorRef, MSet[FOLClause]] with MultiMap[ActorRef, FOLClause]
    mapping         
  }
 }