package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:22:17
 */

case class Variable(name: String) extends Term {

  // is not complex, therefore no arguments
  override val top = name
  override val args = List(this)

  override def toString = name

  override def flatArgs: List[FOLNode] = List(this)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    f(this)
  }


}

object Variable {
  //val randomizer: Random = new Random(System.currentTimeMillis)
  var defaultVariableCounter = 0

  // default factory method
  def apply(): Variable = {
    defaultVariableCounter += 1
    Variable("x_" + defaultVariableCounter)
  }






}

case class IndicatorVariable(override val name: String) extends Variable(name) {
  // make default constructor private

  override val top = name
  override val args = List(this)

  override def toString = name

  override def flatArgs: List[FOLNode] = List(this)

  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    f(this)
  }

}

object IndicatorVariable {
  //val randomizer: Random = new Random(System.currentTimeMillis)

  // default factory method
  def apply(index: Int): IndicatorVariable = {
    IndicatorVariable("*_" + index)
  }

  def rangeTo(ceiling: Int): List[IndicatorVariable] = {
    (for (index <- 0 until ceiling)
    yield IndicatorVariable(index)).toList
  }

  def rangeFromTo(floor: Int,ceiling: Int): List[IndicatorVariable] = {
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



