package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:53:52
 */


trait Sentence extends FOLNode


object Sentence {
  implicit def sentenceToAndConnective(x: Sentence): AndConnective = x.asInstanceOf[AndConnective]

  implicit def sentenceToTerm(x: Sentence): Term = x.asInstanceOf[Term]

  implicit def folnodeToSentence(x: FOLNode): Sentence = x.asInstanceOf[Sentence]

}
