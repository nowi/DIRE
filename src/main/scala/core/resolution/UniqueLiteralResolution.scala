package core.resolution


import caches.MaxLitCache
import domain.fol.ast.{PositiveFOLLiteral, NegativeFOLLiteral, FOLClause, FOLNode}
import helpers.Logging
import ordering.LiteralComparison
import selection.LiteralSelection

/**
 * User: nowi
 * Date: 08.03.2010
 * Time: 12:08:11
 */

trait UniqueLiteralResolution {
  def apply(clause: FOLClause): Option[FOLNode]
}

class DALCUniqueLiteralResolver(env: {val selector: LiteralSelection;val literalComparator : LiteralComparison;val maxLitCache : MaxLitCache}) extends UniqueLiteralResolution with Logging{
  val selector = env.selector
  val literalComparator = env.literalComparator
  implicit val maxLitCache : MaxLitCache = env.maxLitCache

  override def apply(clause: FOLClause) : Option[FOLNode] = {
    // get selected literals

    val selectedLits = selector.selectedLiterals(clause)



    selectedLits match {
      case selectedLit :: List() => { // only one selected
        // resolve upon selectedLit
        // a is main premise
        Some(selectedLit)
      }
      case selectedLit :: moreSelectedLits => { // there are multiple selected lits
        // should not happen
        throw new IllegalStateException("Cannot have more than one selected literal, are you really using the right resolver for the input ?")
      }
      case Nil => { // there is no selected lit
        val compare = literalComparator.compare(_, _)
        // is there is ONE strictly maximal literal A ?

        val maxLiterals: List[FOLNode] = clause.maxLits(literalComparator,maxLitCache)


        log.debug("Clause %s ... Strictly maximal literals are : %s", clause, maxLiterals)


        maxLiterals match {
          case Nil => {
            // there are no maximal literals , should this happen ?
            error("there are no maximal literals , should this happen ?")

          }

          case maxLit :: Nil => {
            // resolve upon strictly maxLit A
            maxLit match {
              case PositiveFOLLiteral(lit) => {
                // clause a is side premise
                Some(maxLit)
              }
              case NegativeFOLLiteral(lit) => {
                // clause a is main premise
                Some(maxLit)
              }
            }

          }

          case maxLit :: maxLits => {
            // a is main premise !

            // check if we can find a negative maxLit
            (maxLit :: maxLits).find(_ match {
              case PositiveFOLLiteral(literal) => false
              case NegativeFOLLiteral(literal) => true
            }) match {
              case Some(negativeMaxLit) => {
                // resolve upon a maximal negaive literal **
                Some(negativeMaxLit)
              }
              case None => {
                // If all multiple maximal literals are positive only factoring is possible
                // because the clause can neither be a side premise nor a main premise
                None
              }
            }

          }
        }


      }
    }
  }
}