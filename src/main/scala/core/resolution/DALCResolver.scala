package core.resolution


import containers.{MatchingClausesRetrieval, CNFClauseStore, ClauseStorage}
import domain.fol.ast._
import helpers.Logging
import ordering.{ALCLPOComparator, LiteralComparison}
import reduction.Factoring
import rewriting.Substitution
import selection.{DALCRSelector, LiteralSelection}

/**
 * User: nowi
 * Date: 01.03.2010
 * Time: 15:32:54
 */

class DALCResolver(env: {val useIndexing: Boolean; val recordProofSteps: Boolean; val unificator: Unify; val factorizer: Factoring; val standardizer: Standardizing; val substitutor: Substitution; val selector: LiteralSelection; val literalComparator: LiteralComparison}) extends Resolution with Logging {
  val unificator = env.unificator
  val factorizer = env.factorizer
  val standardizer = env.standardizer
  val substitutor = env.substitutor
  val selector = env.selector
  val literalComparator = env.literalComparator
  val recordProofSteps = env.recordProofSteps
  val useIndexing = env.useIndexing

  // some invariants , dalc resolver needs compatible selection and comperator

  //require(literalComparator.isInstanceOf[ALCLPOComparator])

  //require(selector.isInstanceOf[ALCRSelector])



  override def resolve(a: FOLClause, b: ClauseStorage) = {
    b match {
      case store: MatchingClausesRetrieval if (useIndexing) => resolveWithMatchingIndex(a, store)
      case _ => resolveWithoutIndex(a, b)
    }


  }

  private def resolveWithoutIndex(a: FOLClause, b: ClauseStorage) = {
    log.trace("Resolving %s with %s", a, b)
    // resolve
    val resolvents = (for (clause2 <- b;
                           if (a != clause2);
                           resolvent = CNFClauseStore(resolve(a, clause2)))
    yield resolvent)

    resolvents match {
      case x if (x.size == 0) => CNFClauseStore()
      case x => x.reduceLeft(_ ::: _)

    }


  }

  private def resolveWithMatchingIndex(a: FOLClause, b: MatchingClausesRetrieval): ClauseStorage = {
    log.trace("Resolving with MatchingIndexSupport %s with %s", a, b)


    // get the matching claueses , substruct the query clause a
    val matchingClauses = a.literals.map({lit: FOLNode => b.getMatchingClauses(lit).getOrElse(Set()) ++ b.getMatchingClauses(Negation(lit)).getOrElse(Set())}).reduceLeft(_ ++ _) - a

    matchingClauses match {
      case clauses: Set[FOLClause] if (clauses.isEmpty) => CNFClauseStore()
      case clauses: Set[FOLClause] => {
        val resolvents = (for (clause2 <- clauses;
                               if (a != clause2);
                               resolvent = CNFClauseStore(resolve(a, clause2)))
        yield resolvent)

        resolvents match {
          case x if (x.size == 0) => CNFClauseStore()
          case x => x.reduceLeft(_ ::: _)

        }

      }

    }


  }


  override def resolve(a: ClauseStorage, b: ClauseStorage): ClauseStorage = {
    log.trace("Resolving %s with %s", a, b)
    // resolve


    (for (clause1 <- a;
          clause2 <- b;
          if (clause1 != clause2);
          resolvent = CNFClauseStore(resolve(clause1, clause2)))
    yield resolvent).reduceLeft(_ ::: _)

  }


  /**
   * Binary Distribtued ALC Resolution method as described in Paper :
   * SCHLICHT und Stuckenschmidt. Distributed resolution for ALC. Proceedings of the International … (2008)
   *
   *
   */
  override def resolve(a: FOLClause, b: FOLClause): Set[FOLClause] = {

    // optimisation with regards to FOL resolution :
    // Analysing all possible types of inferences among these clause will reveal that
    // in all cases the resolvable literals can be determined prior to substitution.

    log.trace("Resolving the Clauses %s,%s", a, b)

    // TODO CEHCK THIS
    val (aStand, bStand, renamings) = standardizer.standardizeApart(a, b)

    // filter out option clauses
    val conclusions: Set[FOLClause] =
    (doResolve(aStand, bStand) // resolve one way
            ++ doResolve(bStand, aStand)) // resolve other way
            .filter(_.isDefined) // filter out options
            .map({_.get}) // convert Option[Map] --> Map


    if (!conclusions.isEmpty)
      log.debug("%s + %s --> %s" format (a, b, conclusions))

    // check if there where renamings , if so reverse them
    val renamedConclusions = if (renamings.isEmpty)
      conclusions
    else
      conclusions.map(substitutor.substitute(Some(renamings), _))

    // log clauses
    if (recordProofSteps) {
      renamedConclusions.foreach(addInferredClause(a, b, _))
    }

    renamedConclusions


  }


  private def doResolve(a: FOLClause, b: FOLClause): Set[Option[FOLClause]] = {
    // check the resolvability

    // first determine the resolvable literal

    // get selected literals

    val selectedLits = selector.selectedLiterals(a)
    



    log.debug("Clause %s ... Selected literals are : %s", a, selectedLits)

    val uniqueResolvableLiteral: Option[FOLNode] = selectedLits match {
      case selectedLit :: List() => { // only one selected
        // resolve upon selectedLit
        // a is main premise
        Some(selectedLit)
      }
      case selectedLit :: moreSelectedLits => { // there are multiple selected lits
        // should not happen
        throw new IllegalStateException("Cannot have more than one selected literal")
      }
      case Nil => { // there is no selected lit
        val compare = literalComparator.compare(_, _)
        // is there is ONE strictly maximal literal A ?

        val maxLits: List[FOLNode] = a.maxLits(literalComparator)

       
        log.debug("Clause %s ... Strictly maximal literals are : %s", a, maxLits)


        maxLits match {
          case Nil => {
            // there are no maximal literals , should this happen ?
            log.error("there are no maximal literals , should this happen ?")
            None

          }

          case maxLit :: Nil => {
            // resolve upon maxLit A
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

            // all maximal lits are positive  ?

            (maxLit :: maxLits).forall(_ match {
              case PositiveFOLLiteral(literal) => true
              case NegativeFOLLiteral(literal) => false
            }) match {
              case true => {
                // clause a is no premise for resolution
                None
              }
              case false => {
                // resolve upon a maximal negaive literal **
                log.warning("resolve upon a maximal negaive literal **")
                Some(maxLit)
              }
            }

          }
        }


      }
    }


    // ouptut the uniqueResolvableLiteral

    log.info("URLit for clause : %s == %s ", a, uniqueResolvableLiteral)


    Set(None)

  }

}
