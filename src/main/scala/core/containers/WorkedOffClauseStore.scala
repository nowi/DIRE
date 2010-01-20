package core.containers


import collection.mutable.ListBuffer
import domain.fol.ast.{EmptyClause, FOLClause, FOLNode}

/**
 * User: nowi
 * Date: 20.12.2009
 * Time: 14:12:56
 */

class WorkedOffClauseStore extends ClauseStorage
        with MatchingClausesRetrieval {

  // internal storage as list buffer
  private var clauses: ListBuffer[FOLClause] = new ListBuffer

  override def isEmpty: Boolean = {
    clauses.isEmpty
  }


  def apply(t: Int) = {
    clauses.apply(t)

  }

  override def elements = clauses.elements

  override def length = clauses.length


  override def head = clauses.first

  override def tail = {
    clauses.drop(1)
    this
  }

  // the index class
  val matchingClausesIndex: FOLClauseIndex = new NaiveFOLClauseIndex


  override def ::(x: FOLClause) = {
    matchingClausesIndex.insert(x)
    clauses.append(x)
    this
  }


  def :::(prefix: ClauseStorage) = {
    val b = new ListBuffer[FOLClause]
    var those = prefix
    while (!those.isEmpty) {
      b += those.head
      matchingClausesIndex.insert(those.head)
      those = those.tail
    }
    clauses.appendAll(b)
    this
  }


  override def filterClauses(f: Function1[FOLClause, Boolean]): ClauseStorage = {
    val filtered = new ListBuffer[FOLClause]
    filtered.appendAll(clauses.toList.filter(f))
    clauses = filtered
    this
  }


  override lazy val containsEmptyClause: Boolean =
  (clauses exists ((_ match {
    case EmptyClause() => true
    case _ => false
  })))

  override def toString = "WorkedOff Clause Store : %s" format (clauses mkString ("(", "\n", ")"))


  override def getMatchingClauses(node: FOLNode) = {
    matchingClausesIndex.retrieve(node)

  }
}