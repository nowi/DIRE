package core


import domain.fol.ast.{FOLNode, Variable}

/**
 * User: nowi
 * Date: 04.11.2009
 * Time: 17:15:12
 */

trait MGU extends Map[Variable, FOLNode]

object MGU {
  implicit def mapToMGU(x: Map[Variable, FOLNode]): MGU = x.asInstanceOf[MGU]

  implicit def optionmapTooptionMGU(x: Option[Map[Variable, FOLNode]]): MGU = x.asInstanceOf[Option[MGU]]

}
