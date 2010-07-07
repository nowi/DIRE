package de.unima.dire.core.containers


import collection.immutable.{TreeMap, SortedMap}
import collection.mutable._
import de.unima.dire.domain.fol.ast.{FOLNode}
import de.unima.dire.helpers.Logging
import scala.collection.mutable.{Set => MSet}
import collection.Iterable

/**
 * User: nowi
 * Date: 30.04.2010
 * Time: 17:42:02
 */


trait ListBufferStorage extends MutableClauseStorage with Logging {
  this: MutableClauseStorage =>
  // intercept removeNext

  // default structure is a simlpe listbuffer
  val buffer = new ListBuffer[FOLClause]()




  // allow access to this map from traits that are mixed in after this trait
  override val termToClause: scala.collection.mutable.MultiMap[FOLNode, FOLClause] =
  new HashMap[FOLNode, MSet[FOLClause]] with MultiMap[FOLNode, FOLClause]


  val st = (a: FOLClause, b: FOLClause) => a.size < b.size




  // this can be overriden by the mixed in heuristic !
  override def size = buffer.size

  override def toList = buffer.readOnly.toList

  def isEmpty = buffer.isEmpty


  override def hasNext = !buffer.isEmpty

  override def removeNext: FOLClause = {
    if (isEmpty) throw new NoSuchElementException
    val clause = buffer.head

    for (term <- clause.literals) {
      // remove from termToClause
      termToClause.removeBinding(term, clause)
    }
    // remove this clause
    buffer -= clause
    clause
  }


  override def remove(clause: FOLClause) = {
    for (term <- clause.literals) {
      // remove from termToClause
      termToClause.removeBinding(term, clause)
    }

    buffer -= clause
    clause
  }

  override def removeAll(clauses: Iterable[FOLClause]) : Unit = {
    clauses.foreach(remove _)
  }


  override def addAll(clauses: Iterable[FOLClause]) : Unit = {
    clauses.foreach(add _)
  }




  def contains(clause: FOLClause) = {
    val termToClauseValues = termToClause.values.toList.flatten(trs => trs)
    val isInTerm2Clause = termToClauseValues.contains(clause)
    isInTerm2Clause
  }


  override def add(a: FOLClause): Unit = {
        buffer += a

    for (literal <- a.literals) {
      // associate this literal with the clause
      termToClause.addBinding(literal, a)
    }
  }

}



trait LightestClauseHeuristicStorage extends MutableClauseStorage with Logging {
  this: MutableClauseStorage =>
  // intercept removeNext
  // the storage , clauses sorted with lightes clause heuristic , in size buckets

  val ratio: Int = 5000
  var counter: Int = 0

  var buckets: SortedMap[Int, Queue[FOLClause]] = new TreeMap[Int, Queue[FOLClause]]()

  // allow access to this map from traits that are mixed in after this trait
  override val termToClause: scala.collection.mutable.MultiMap[FOLNode, FOLClause] =
  new HashMap[FOLNode, MSet[FOLClause]] with MultiMap[FOLNode, FOLClause]


  //override def self = elements.toList

  private var _size: Int = 0


  // we manage our size explicitly , no delegation to underlying strcutures
  // we will count every addition and removal of clauses




  override def hasNext = {
    val notEmpty = (buckets.filter({t => !t._2.isEmpty})).asInstanceOf[SortedMap[Int, Queue[FOLClause]]] match {
      case x if (!x.isEmpty) => Some(x)
      case _ => None
    }

    notEmpty match {
      case Some(neb) => true
      case None => false
    }
  }

  override def size = _size


  def isEmpty = size == 0

  override def toList = {
    if (isEmpty) Nil
    else
      termToClause.values.toList.flatten(itr => itr)

  }


//  def elements = {
//    val notEmpty = (buckets.filter({t => !t._2.isEmpty})).asInstanceOf[SortedMap[Int, Queue[FOLClause]]] match {
//      case x if (!x.isEmpty) => Some(x)
//      case _ => None
//    }
//
//    notEmpty match {
//      case Some(neb) => Iterator.flatten(neb.values.map {q: Queue[FOLClause] => q.elements.asInstanceOf[Iterator[FOLClause]]})
//      case None => Iterator.empty
//      case null => Iterator.empty
//    }
//
//  }

  // TODO expensive
  def contains(clause: FOLClause) = {
    //    val isInBucket = buckets.get(clause.size) match {
    //      case Some(bucket: Queue[FOLClause]) => {
    //        // check if its in queue
    //        bucket.contains(clause)
    //      }
    //      case None => false
    //    }

    val termToClauseValues = termToClause.values.toList.flatten(trs => trs)
    val isInTerm2Clause = termToClauseValues.contains(clause)



    isInTerm2Clause

  }


  override def add(clause: FOLClause) = {
    // check if this clause is not already contained
    // check in both index and directly in buckets
    // TODO expensive !
    val bucketIndex = clause.size
    buckets.get(bucketIndex) match {
      case Some(bucket: Queue[FOLClause]) => {
        // TODO check this , this avoids insertion of claues that are already in the storage
        // this should not happen in the first place but unfortunately does, still investigating why
        // the contains check is expensive as long as we have not switched to shared term representations
        // whene terms are shared contains will beenfit from pointer adress comparison instead of deep equality checks
        // as it is now
        bucket.enqueue(clause)
        for (literal <- clause.literals) {
          // associate this literal with the clause
          termToClause.addBinding(literal, clause)
        }

        _size += 1
      }
      case None => {
        // craete the bucket
        val bucket = new Queue[FOLClause]()
        bucket.enqueue(clause)
        for (literal <- clause.literals) {
          // associate this literal with the clause
          termToClause.addBinding(literal, clause)
        }
        _size += 1
        //  TODO this stinks
        buckets = buckets.asInstanceOf[TreeMap[Int, Queue[FOLClause]]].insert(bucketIndex, bucket)
      }
    }


  }



  override def removeAll(clauses: Iterable[FOLClause]): Unit = {
    clauses.foreach(remove _)

  }


  def addAll(clauses: Iterable[FOLClause]) : Unit = {
    clauses.foreach(add _)
  }

  override def remove(clause: FOLClause): FOLClause = {
    // TODO optimize this , create dedicated removeALl method to minimize buffer copying

    //    if (clause.toString == "[¬(O1Poster(U))]") {
    //      log.warning("Big backward sumsumption is comming with [¬(O1Poster(U))]")
    //    }

    val key = clause.size
    buckets.get(key) match {
      case Some(queue) => {
        // remove the clause from the qeuue
        val sizeBefore = queue.size
        val newQueue: Queue[FOLClause] = new Queue()
        queue.filter(_ != clause).foreach(newQueue.enqueue(_))


        //  TODO this stinks
        buckets = buckets.updated(key, newQueue).asInstanceOf[SortedMap[Int, Queue[FOLClause]]]

        //        val sizeAfter = buckets.get(key).get.size
        //        if(sizeBefore - 1 != sizeAfter){
        //          log.error("Bucket size mismatch after delete ")
        //        }

        // remove from lookup map
        for (term <- clause.literals) {
          // remove from termToClause
          termToClause.removeBinding(term, clause)
        }

        _size -= 1

        clause

      }
      case None => {
        // there is no bucket
        throw new NoSuchElementException
      }
    }


  }


  override def removeNext(): FOLClause = {
    val notEmpty = (buckets.filter({t => !t._2.isEmpty})).asInstanceOf[SortedMap[Int, Queue[FOLClause]]] match {
      case x if (!x.isEmpty) => Some(x)
      case _ => None
    }
    notEmpty match {
      case Some(neb) => {
        // TODO extend subclasses with paramterized pick / given ratios


        val key: Int = neb.firstKey

        neb.get(key) match {
          case Some(queue) => {
            val clause = queue.dequeue
            // remove from lookup map
            for (term <- clause.literals) {
              // remove from termToClause
              termToClause.removeBinding(term, clause)
            }

            _size -= 1
            clause

          }
          case None => {
            // there is no bucket
            throw new NoSuchElementException
          }
        }
      }
      case None => throw new NoSuchElementException
    }

  }

}