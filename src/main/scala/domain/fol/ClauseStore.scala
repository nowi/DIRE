package domain.fol


import ast.Clause

/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 */

abstract class ClauseStore {
}

/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * Conjunctive Normal Form (CNF) : a conjunction of clauses, where each
 * clause is a disjunction of literals.
 *
 */
case class CNFClauseStore(clauses: List[Clause]) extends ClauseStore {
}

/**
 * User: nowi
 * Date: 08.10.2009
 * Time: 09:41:59
 *
 * Disjunctive Normal Form (CNF) : a disjunction of clauses, where each
 * clause is a disjunction of literals.
 *
 */
case class DNFClauseStore(clauses: List[Clause]) extends ClauseStore {
}