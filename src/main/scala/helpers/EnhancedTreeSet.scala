package helpers


import collection.immutable.{RedBlack, TreeSet}

/**
 * User: nowi
 * Date: 05.12.2009
 * Time: 15:32:58
 */


class EnhancedTreeSet[A <% Ordered[A]](val s1: Int, t1: RedBlack[A]#Tree[Unit]) extends TreeSet[A](s1, t1) {
  def this() = this (0, null)

  override def --(elems: Iterable[A]): EnhancedTreeSet[A] = {
    var newTree: RedBlack[A]#Tree[Unit] = tree
    var newSize = size
    for (elem <- elems) {
      if (!tree.lookup(elem).isEmpty) {
        newSize -= 1
        newTree = newTree.delete(elem)
      }
    }
    if (newSize != size) {
      // there have been deletions
      new EnhancedTreeSet[A](newSize, newTree)
    } else
      this

  }

  override def ++(elems: Iterable[A]): EnhancedTreeSet[A] = {
    var newTree: RedBlack[A]#Tree[Unit] = tree
    var newSize = size
    for (elem <- elems) {
      if (tree.lookup(elem).isEmpty) {
        newSize += 1
        newTree = newTree.update(elem, ())
      }
    }
    if (newSize != size) {
      // there have been deletions
      new EnhancedTreeSet[A](newSize, newTree)
    } else
      this

  }
}
