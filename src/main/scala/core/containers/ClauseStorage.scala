package core.containers

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 19:29:15
 */

import domain.fol.ast.{FOLNode, FOLClause, Sentence}

/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * A clause store is a multiset of clauses
 *
 */
trait ClauseStorage extends Seq[FOLClause] {
  def values: Set[FOLClause] = Set(elements.toList: _*)

  override def hashCode = values.hashCode

  override def equals(that: Any): Boolean = that match {
    case other: ClauseStorage =>
      this.size == other.size && values == other.values
    case _ =>
      false
  }


  val containsEmptyClause: Boolean

  /**Returns this first element of the list.
   *
   * @return the first element of this list.
   * @throws Predef.NoSuchElementException if the list is empty.
   */
  def head: FOLClause


  /**Returns this first element of the list.
   *
   * @return the first element of this list.
   * @throws Predef.NoSuchElementException if the list is empty.
   */
  def headOption: Option[FOLClause]

  /**returns length - l, without calling length
   */
  override def lengthCompare(l: Int) = {
    if (isEmpty) 0 - l
    else if (l <= 0) 1
    else tail.lengthCompare(l - 1)
  }

  /**Returns this list without its first element.
   *
   * @return this list without its first element.
   * @throws Predef.NoSuchElementException if the list is empty.
   */
  def tail: ClauseStorage


  /**<p>
   *    Add an element <code>x</code> at the beginning of this list.
   *  </p>
   *
   * @param x the element to append.
   * @return the list with <code>x</code> appended at the beginning.
   * @ex <code>1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)</code>
   */
  def ::(x: FOLClause): ClauseStorage

  /**<p>
   *    Returns a list resulting from the concatenation of the given
   *    list <code>prefix</code> and this list.
   *  </p>
   *
   * @param prefix the list to concatenate at the beginning of this list.
   * @return the concatenation of the two lists.
   * @ex <code>List(1, 2) ::: List(3, 4) = List(3, 4).:::(List(1, 2)) = List(1, 2, 3, 4)</code>
   */
  def :::(prefix: ClauseStorage): ClauseStorage


  def filterClauses(f: Function1[FOLClause, Boolean]): ClauseStorage
}