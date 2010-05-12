package core.resolution


import collection.immutable.Set
import containers.ClauseStorage
import domain.fol.ast.{FOLNode, FOLClause}
/**
 * User: nowi
 * Date: 18.03.2010
 * Time: 11:44:14
 */



sealed trait ResolutionResult extends InferenceResult {
  def result : Any
}

sealed trait BinaryResolutionResult extends ResolutionResult {
  def result : List[FOLNode]
}


sealed trait ReductionResult {
  val result : List[FOLNode]

}


case class SuccessfullResolution(override val result : List[FOLNode],val parent1:FOLClause,val parent2:FOLClause) extends BinaryResolutionResult {
}

case class FailedResolution(val parent1:FOLClause,val parent2:Option[FOLClause]) extends BinaryResolutionResult {
  override val result = Nil
}

case class SuccessfullPositiveFactoring(val result : List[FOLNode],val parent1:FOLClause) extends InferenceResult {

}

case class FailedPositiveFactoring(val parent:FOLClause) extends InferenceResult {
  val result = Nil
}




case class SuccessfullReduction(override val result : List[FOLNode],val parent1:FOLClause,val parent2:FOLClause) extends ReductionResult {
}


case class FailedReduction(val parent1:FOLClause,val parent2:Option[FOLClause]) extends ReductionResult {
  override val result = Nil
}

