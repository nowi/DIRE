package core.containers


import collection.immutable.TreeMap
import collection.mutable.{Queue, MutableList}
import collection.SortedMap
import domain.fol.ast.{FOLNode, FOLClause}
import java.util.NoSuchElementException
import javax.naming.OperationNotSupportedException

/**Usable Clause Store uses the LightestClauseHeuristic by storing all clauses with same
 * literal count into dedicated clause queues
 * User: nowi
 * Date: 16.12.2009
 * Time: 18:31:16
 */

class UsableClauseStore(c: List[FOLClause])
        extends CNFClauseStore(c)
                with MatchingClausesRetrieval {


  // the storage , clauses sorted with lightes clause heuristic , in size buckets
  var buckets: SortedMap[Int, Queue[FOLClause]] = TreeMap[Int, Queue[FOLClause]]()

  // the index class
  val matchingClausesIndex: FOLClauseIndex = new NaiveFOLClauseIndex

  // index the initial clauses
  for (clause <- c) matchingClausesIndex.insert(clause)



  // get from index
  override def apply(t: Int) = {
    elements.toList.apply(t)
  }

  override def elements = {
    nonEmptyBuckets match {
      case Some(neb) => Iterator.flatten(neb.values.map {q: Queue[FOLClause] => q.elements})
      case None => Iterator.empty
    }

  }




  // TODO check this smells like bad design
  // not supported would require index filtering ...
  override def filterClauses(f: Function1[FOLClause, Boolean]) = {
    // filterover all clauses
    throw new OperationNotSupportedException
  }


  override def length = elements.toList.size


  override def head: FOLClause = headOption match {
    case Some(clause) => clause
    case None => throw new NoSuchElementException

  }


  override def tail = {
    // dequeue the head
    dequeue
    this

  }


  override def isEmpty = headOption match {
    case Some(clause) =>false
    case _  => true
  }


  // get the next clause , default only ligites clause, override this if
  // you want more sophistication e.g. pick/given ratio control
  override def headOption: Option[FOLClause] = {
    nonEmptyBuckets match {
      case Some(neb) => {
        neb.get((neb.firstKey)) match {
          case Some(queue: Queue[FOLClause]) => {
            queue.firstOption

          }
          case None => {
            // there is no bucket
            None
          }
        }

      }

      case None => None
    }
  }


  override def :::(prefix: ClauseStorage) = {
    enqueue(prefix: _*)
    // index
    matchingClausesIndex.insertAll(prefix)
    this
  }


  override def ::(x: FOLClause) = {
    enqueue(x)
    // index
    matchingClausesIndex.insert(x)
    this

  }


  def enqueue(clauses: FOLClause*) {
    // add to clauses at bucket
    for (clause <- clauses) {
      insert(clause, clause.size)
      // index
      matchingClausesIndex.insert(clause)
    }

  }


  /*
  special enquee for clause storage structures , maybe we could copy indexes if
  compatible , for now delegate to default enqueue
   */
  def enqueue(clauses: ClauseStorage) {
    for (clause <- clauses) {
      insert(clause, clause.size)
      // index
      matchingClausesIndex.insert(clause)
    }
    this

  }


  def dequeue() = {
    nonEmptyBuckets match {
      case Some(neb) => {
        neb.get((neb.firstKey)) match {
          case Some(queue) => {
            val clause = queue.dequeue
            // deindex
            matchingClausesIndex.delete(clause)
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


  protected def insert(x: FOLClause, bucketIndex: Int) {
    // get the bucket
    buckets.get(bucketIndex) match {
      case Some(bucket: Queue[FOLClause]) => {
        bucket.enqueue(x)
      }
      case None => {
        // craete the bucket
        val bucket = new Queue[FOLClause]()
        bucket.enqueue(x)
        //        buckets =+ .asInstanceOf[TreeMap[Int, Queue[B]]].insert(bucketIndex, bucket)
        buckets = buckets.asInstanceOf[TreeMap[Int, Queue[FOLClause]]].insert(bucketIndex, bucket)
      }
    }
  }


  private def nonEmptyBuckets: Option[SortedMap[Int, Queue[FOLClause]]] = {
    val filtered = (buckets.filter({t => !t._2.isEmpty})).asInstanceOf[SortedMap[Int, Queue[FOLClause]]]
    filtered match {
      case x if (x.isEmpty) => None
      case _ => Some(filtered)
    }

  }



  //////// INDEX RETRIEVALS ////////////////

  override def getMatchingClauses(node: FOLNode) = {
    // use the matching index
    matchingClausesIndex.retrieve(node)
  }
}