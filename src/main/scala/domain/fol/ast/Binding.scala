package domain.fol.ast

/**
 * User: nowi
 * Date: 31.03.2010
 * Time: 15:50:15
 */

case class Binding(v : Variable,t : FOLNode) extends Tuple2[Variable,FOLNode](v,t) {

}