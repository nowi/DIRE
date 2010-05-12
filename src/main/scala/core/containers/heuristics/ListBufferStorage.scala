package core.containers.heuristics


import collection.immutable.{TreeMap, SortedMap}
import collection.mutable._
import domain.fol.ast.{FOLNode, FOLClause}
import helpers.Logging
import scala.collection.mutable.{Set => MSet}

/**
 * User: nowi
 * Date: 30.04.2010
 * Time: 17:42:02
 */


trait ListBufferStorage extends MutableClauseStorage {
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
    val clause = buffer.first

    for (term <- clause.literals) {
      // remove from termToClause
      termToClause.remove(term, clause)
    }
    // remove this clause
    buffer -= clause
    clause
  }


  def remove(clause: FOLClause) = {
    for (term <- clause.literals) {
      // remove from termToClause
      termToClause.remove(term, clause)
    }

    buffer -= clause
    clause
  }

  override def add(a: FOLClause): Unit = {
    // append the element to the buffer in O(1)
    buffer += a

    for (literal <- a.literals) {
      // associate this literal with the clause
      termToClause.add(literal, a)
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
      termToClause.readOnly.values.toList.flatten(itr => itr)

  }


  def elements = {
    val notEmpty = (buckets.filter({t => !t._2.isEmpty})).asInstanceOf[SortedMap[Int, Queue[FOLClause]]] match {
      case x if (!x.isEmpty) => Some(x)
      case _ => None
    }

    notEmpty match {
      case Some(neb) => Iterator.flatten(neb.values.map {q: Queue[FOLClause] => q.elements})
      case None => Iterator.empty
      case null => Iterator.empty
    }

  }


  override def add(clause: FOLClause) {




    // get the bucket
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
          termToClause.add(literal, clause)
        }
        _size += 1
      }
      case None => {
        // craete the bucket
        val bucket = new Queue[FOLClause]()
        bucket.enqueue(clause)
        for (literal <- clause.literals) {
          // associate this literal with the clause
          termToClause.add(literal, clause)
        }
        _size += 1
        //  TODO this stinks
        buckets = buckets.asInstanceOf[TreeMap[Int, Queue[FOLClause]]].insert(bucketIndex, bucket)
      }
    }

  }

  private def checkSizeInvariant {
    //    val term2clauseSize =  termToClause.values.toList.flatten(itr => itr).removeDuplicates.size

  }

  override def remove(clause: FOLClause): FOLClause = {
    // TODO optimize this , create dedicated removeALl method to minimize buffer copying
    val key = clause.size
    buckets.get(key) match {
      case Some(queue) => {
        // remove the clause from the qeuue
        val newQueue = queue.filter(_ != clause)

        buckets.update(key, newQueue)

        // remove from lookup map
        for (term <- clause.literals) {
          // remove from termToClause
          termToClause.remove(term, clause)
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
        //        val key : Int = if(counter % ratio == 0) {
        //          counter = 0
        //          neb.lastKey
        //        } else {
        //          neb.firstKey
        //        }
        //        counter = counter + 1

        val key: Int = neb.firstKey

        neb.get(key) match {
          case Some(queue) => {
            val clause = queue.dequeue
            // remove from lookup map
            for (term <- clause.literals) {
              // remove from termToClause
              termToClause.remove(term, clause)
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
