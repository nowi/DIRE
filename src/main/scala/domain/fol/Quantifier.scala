package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:23:55
 */

abstract class Quantifier extends Sentence


case class UniversalQuantifer(filler: Sentence, variables: Variable*) extends Quantifier
case class ExistentialQuantifer(filler: Sentence, variables: Variable*) extends Quantifier