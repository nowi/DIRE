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
  def result : Set[FOLNode]
}


sealed trait ReductionResult {
  val result : Set[FOLNode]

}


case class SuccessfullResolution(override val result : Set[FOLNode],val parent1:FOLClause,val parent2:FOLClause) extends BinaryResolutionResult {
}

case class FailedResolution(val parent1:FOLClause,val parent2:Option[FOLClause]) extends BinaryResolutionResult {
  override val result = Set.empty[FOLNode]
}

case class SuccessfullPositiveFactoring(val result : Set[FOLNode],val parent1:FOLClause) extends InferenceResult {

}

case class FailedPositiveFactoring(val parent:FOLClause) extends InferenceResult {
  val result = Set.empty[FOLNode]
}




case class SuccessfullReduction(override val result : Set[FOLNode],val parent1:FOLClause,val parent2:FOLClause) extends ReductionResult {
}


case class FailedReduction(val parent1:FOLClause,val parent2:Option[FOLClause]) extends ReductionResult {
  override val result = Set.empty[FOLNode]
}

