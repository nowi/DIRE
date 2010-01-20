package helpers

/**
 * User: nowi
 * Date: 04.12.2009
 * Time: 19:41:55
 */

class IteratorWrapper[A](iter: java.util.Iterator[A])
 {
  def foreach(f: A => Unit): Unit = {
    while (iter.hasNext) {
      f(iter.next)
    }
  }
}