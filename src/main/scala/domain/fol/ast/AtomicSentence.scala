package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:53:45
 */

trait AtomicSentence extends Sentence


object AtomicSentence {
  implicit def atomicSentenceToTerm(x: AtomicSentence): Term = x.asInstanceOf[Term]

}
