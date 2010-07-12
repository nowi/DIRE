package core.containers.heuristics


import collection.immutable.{TreeMap, SortedMap}
import collection.mutable._
import domain.fol.ast.{FOLNode, FOLClause}
import helpers.Logging
import reduction.{StillmannSubsumer, ForwardSubsumer}
import scala.collection.mutable.{Set => MSet}

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

  override def removeNext: FOLClause = synchronized {
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


  override def remove(clause: FOLClause) = synchronized {
    for (term <- clause.literals) {
      // remove from termToClause
      termToClause.remove(term, clause)
    }

    buffer -= clause
    clause
  }

  override def removeAll(clauses: Iterable[FOLClause]) = synchronized {
    clauses.foreach(remove _)
  }

  // TODO expensive
  def contains(clause: FOLClause) = synchronized {
    val termToClauseValues = termToClause.values.toList.flatten(trs => trs)
    val isInTerm2Clause = termToClauseValues.contains(clause)
    isInTerm2Clause
  }


  override def add(a: FOLClause): Unit = synchronized {
    //    if (ForwardSubsumer(a, this)(StillmannSubsumer)) {
    //      log.error("ListBufferStorage tries to add a clause that is already subsumed: %s", a)
    //    } else if (contains(a)) {
    //      // check if forwardsubsumption would catch this clause
    //      log.error("ListBufferStorage tries to add a clause that is NOT SUBsumed but contained ?? cannot happen", a)
    //    } else {
    //
    //    }
    //  }
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




  override def hasNext = synchronized {
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

  override def toList = synchronized {
    if (isEmpty) Nil
    else
      termToClause.readOnly.values.toList.flatten(itr => itr)

  }


  def elements = synchronized {
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

  // TODO expensive
  def contains(clause: FOLClause) = synchronized {
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


  override def add(clause: FOLClause) = synchronized {
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

  //  private def addToLookup(literal : FOLNode,clause : FOLClause) {
  //
  //  }

  private def checkSizeInvariant {
    //    val termToClauseValues = termToClause.values.toList.flatten(trs => trs)
    //    val bucketliterals = elements.toList.flatMap(_.literals).removeDuplicates
    //    val term2clauseLiterals = termToClauseValues.flatMap(_.literals).removeDuplicates
    //    //val term2clausekeyLiterals = termToClause.keys.toList.removeDuplicates
    //    // compare the counts
    //    val bucketliteralsCount = bucketliterals.size
    //    val term2clauseLiteralsCount = term2clauseLiterals.size
    //    //val term2clausekeyLiteralsCount = term2clausekeyLiterals.size
    //    if (!(bucketliteralsCount == term2clauseLiteralsCount)) {
    //      log.error("Size invariants donat match")
    //    }


  }

  override def removeAll(clauses: Iterable[FOLClause]) = synchronized {
    clauses.foreach(remove _)

  }


  override def remove(clause: FOLClause): FOLClause = synchronized {
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
        buckets = buckets.asInstanceOf[TreeMap[Int, Queue[FOLClause]]].update(key, newQueue)

        //        val sizeAfter = buckets.get(key).get.size
        //        if(sizeBefore - 1 != sizeAfter){
        //          log.error("Bucket size mismatch after delete ")
        //        }

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


  override def removeNext(): FOLClause = synchronized {
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
              termToClause.remove(term, clause)
            }

            _size -= 1
            checkSizeInvariant
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