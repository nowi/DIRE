package domain.fol.ast


import helpers.Logging

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:22:17
 */

case class Variable(name: String) extends Term with Logging{

  // is not complex, therefore no arguments
  override lazy val top = name
  override lazy val args = List(this)

  override def toString = name

  override def flatArgs: List[FOLNode] = List(this)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    f(this)
  }


}

object Variable extends Logging{
  //val randomizer: Random = new Random(System.currentTimeMillis)
  var defaultVariableCounter = 0

  // default factory method
  def apply(): Variable = {
    defaultVariableCounter += 1
    Variable("x_" + defaultVariableCounter)
  }

  def nextAuxiliary(variables : List[Variable]) = {

    // TODO need a unique marker variable here
    //val newPathUniqueHash = variables.removeDuplicates.s(_.hashCode).foldLeft(seed)(_ + _)
    val newPathUniqueHash = variables.removeDuplicates.size + 1
    Variable("x_" + newPathUniqueHash)

  }


}

object VariableS {
  // override default apply method in order to implement caching
  def apply(name: String) = {
    // create temp object
    val temp = new Variable(name)
    // return shared representation
    temp.shared
  }
}


case class IndicatorVariable(override val name: String) extends Variable(name) {
  // make default constructor private

  override lazy val top = name
  override lazy val args = List(this)

  override def toString = name

  override def flatArgs: List[FOLNode] = List(this)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    f(this)
  }

}

object IndicatorVariable {
  //val randomizer: Random = new Random(System.currentTimeMillis)

  // default factory method
  def apply(index: Int) = {
    // create temp object
    val temp = new IndicatorVariable("*_" + index)
    // return shared representation
    temp.shared
  }

  def rangeTo(ceiling: Int) = {
    (for (index <- 0 until ceiling)
    yield IndicatorVariable(index)).toList
  }

  def rangeFromTo(floor: Int,ceiling: Int)  = {
    (for (index <- floor until ceiling)
    yield IndicatorVariable(index)).toList
  }


}

// special extractor
object NonIndicatorVariable {
  def unapply(node: FOLNode): Option[Variable] = {
    node match {

      case IndicatorVariable(name) => {
        None
      }

      case v : Variable => {
        Some(v)
      }
      case _ => {
        None
      }
    }
  }
}



