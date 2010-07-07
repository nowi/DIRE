package de.unima.dire.core.caches

/**
 * User: nowi
 * Date: 20.05.2010
 * Time: 21:38:30
 */

import de.unima.dire.helpers.Logging
import de.unima.dire.domain.fol.ast.FOLNode
import de.unima.dire.core.containers.FOLClause


import collection.mutable.{MapProxy, Map => MMap}
import se.scalablesolutions.akka.actor.{ActorRef, Actor}


trait FOLCache

class MaxLitCache extends MapProxy[FOLClause,List[FOLNode]] with Logging{
  override val self = MMap[FOLClause,List[FOLNode]]()

  override def getOrElseUpdate(key: FOLClause, default: => List[FOLNode]): List[FOLNode] =
    get(key) match {
      case Some(v) => {
        v
      }
      case None => {
        val d = default; this(key) = d; d
      }
    }


  override def apply(a: FOLClause) = self.getOrElse(a,Nil)


}

class URLitCache extends MapProxy[FOLClause,Option[FOLNode]] with Logging{
  override val self = MMap[FOLClause,Option[FOLNode]]()

  override def apply(a: FOLClause) = self.getOrElse(a,None)

  /** Check if this map maps <code>key</code> to a value.
    * Return that value if it exists, otherwise put <code>default</code>
    * as that key's value and return it.
    */
  override def getOrElseUpdate(key: FOLClause, default: => Option[FOLNode]): Option[FOLNode] =
    get(key) match {
      case Some(v) => {
        v
      }
      case None => {
        val d = default; this(key) = d; d
      }
    }

}
class SelectedLitCache extends MapProxy[FOLClause,List[FOLNode]]  with Logging{
  override val self = MMap[FOLClause,List[FOLNode]]()

  override def apply(a: FOLClause) = self.getOrElse(a,Nil)

  override def getOrElseUpdate(key: FOLClause, default: => List[FOLNode]): List[FOLNode] =
    get(key) match {
      case Some(v) => {
        v
      }
      case None => {
        val d = default; this(key) = d; d
      }
    }

}

class ActorCache extends MapProxy[String,Option[ActorRef]] with Logging{
  override val self = MMap[String,Option[ActorRef]]()

  override def apply(a: String) = self.getOrElse(a,None)

  /** Check if this map maps <code>key</code> to a value.
    * Return that value if it exists, otherwise put <code>default</code>
    * as that key's value and return it.
    */
  override def getOrElseUpdate(key: String, default: => Option[ActorRef]): Option[ActorRef] =
    get(key) match {
      case Some(v) => {
        v
      }
      case None => {
        val d = default; this(key) = d; d
      }
    }

}


//trait MaxLitCache extends MapProxy[FOLClause,List[FOLNode]] {
//  override val self = MMap[FOLClause,List[FOLNode]]()
//}
//
//trait URLitCache extends  {
//  override val self = MMap[FOLClause,Option[FOLNode]]()
//  override def apply(a: FOLClause) = self.getOrElse(a,None)
//}
//
//trait  SelectedLitCache extends MapProxy[FOLClause,List[FOLNode]]  {
//  override val self = MMap[FOLClause,List[FOLNode]]()
//
//}


