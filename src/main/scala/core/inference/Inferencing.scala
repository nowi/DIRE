package core.inference

import domain.fol.ast._

/**
 * User: nowi
 * Date: 21.10.2009
 * Time: 14:25:14
 */

trait Inferencing {
  // TODO Implement this
  def infer(clause: Clause): Clause
}

class Resolution extends Inferencing {
  // TODO Implement this

  def infer(clause: Clause) = null
}

class FactoringRight extends Inferencing {
  def infer(clause: Clause) = null
}

class FactoringLeft extends Inferencing {
  def infer(clause: Clause) = null
}

