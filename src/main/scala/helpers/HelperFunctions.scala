package helpers


import domain.fol.Substitution
import java.io.{OutputStreamWriter, BufferedOutputStream, FileOutputStream}
import runtime.RichString
/**
 * User: nowi
 * Date: 03.04.2010
 * Time: 13:49:36
 */

object HelperFunctions {
  def convertIterableOfOptions(iter: Iterable[Option[Substitution]]) = {
    if (iter.forall(_ isDefined)) {
      Some(iter.map(_.get))
    } else None
  }


  def permutations[T](list: List[T]): List[List[T]] = list match {
    case Nil => List(List())
    case list => list flatMap ((elem: T) => permutations[T](list remove ((a: T) => a == elem)) map ((b: List[T]) => elem :: b))
  }

  // helper function tp zip together 3 lists ,see http://langref.org/scala/lists/manipulation/list-gather
  def zip3[A, B, C](l1: List[A], l2: List[B], l3: List[C]): List[Tuple3[A, B, C]] =
    {
      def zip3$(l1$ : List[A], l2$ : List[B], l3$ : List[C], acc: List[Tuple3[A, B, C]]): List[Tuple3[A, B, C]] = l1$ match
      {
        case Nil => acc reverse
        case l1$head :: l1$tail => zip3$(l1$tail, l2$.tail, l3$.tail, Tuple3(l1$head, l2$.head, l3$.head) :: acc)
      }

      zip3$(l1, l2, l3, List[Tuple3[A, B, C]]())
    }


  def tupled[a1, a2, a3, a4, a5, a6, b](f: (a1, a2, a3, a4, a5, a6) => b): Tuple6[a1, a2, a3, a4, a5, a6] => b = {
  case Tuple6(x1, x2, x3, x4, x5, x6) => f(x1, x2, x3, x4, x5, x6)
  }



  // dump to file helper

  def dumpToFile(data: String, filename: String) {
    val basePath = System.getProperty("user.dir");

    val os = new OutputStreamWriter((new FileOutputStream(basePath + "/" + filename)))
    os.write(data)
    os.close()
  }


}

class DumpableString(val self: String) extends Proxy with RandomAccessSeq[Char] {
  override def apply(n: Int) = self charAt n
 
  override def length = self.length
  override def toString = self
  override def mkString = self
  
  def >>>(fileName: String) {
    HelperFunctions.dumpToFile(self,fileName)
  }

}


