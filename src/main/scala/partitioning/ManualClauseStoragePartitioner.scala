package partitioning


import core.containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.{FOLNode, FOLClause}
import domain.fol.parsers.SPASSIntermediateFormatParser
import java.io.File

/**
 * User: nowi
 * Date: 21.01.2010
 * Time: 19:48:14
 */

class ManualClauseStoragePartitioner(filenames: List[String]) extends ClauseStoragePartitioning {
  // parser
  // TODO externalize this
  val parser = SPASSIntermediateFormatParser

  override def partition(clauses: ClauseStorage) = {
    val file = new File("input/conf/merged.dire")
    val clauses = SPASSIntermediateFormatParser.parseFromFile(file)
    // clauses is the big merged conf ontology
    // the partitions are O0 O1 O2 O3 O4
    // each predicate has the partitions it belongs to as prefix to its symbolic name

    // partition the clauses accordingly
    val partition0: Set[FOLClause] = Set() ++ clauses.filter({clause: FOLClause => clause.literals.exists(_.symbolicName.substring(0, 2) == "O0")})
    val partition1: Set[FOLClause] = Set() ++ clauses.filter({clause: FOLClause => clause.literals.exists(_.symbolicName.substring(0, 2) == "O1")})
    val partition2: Set[FOLClause] = Set() ++ clauses.filter({clause: FOLClause => clause.literals.exists(_.symbolicName.substring(0, 2) == "O2")})
    val partition3: Set[FOLClause] = Set() ++ clauses.filter({clause: FOLClause => clause.literals.exists(_.symbolicName.substring(0, 2) == "O3")})
    val partition4: Set[FOLClause] = Set() ++ clauses.filter({clause: FOLClause => clause.literals.exists(_.symbolicName.substring(0, 2) == "O4")})

    List(CNFClauseStore(partition0), CNFClauseStore(partition1), CNFClauseStore(partition2), CNFClauseStore(partition3),
      CNFClauseStore(partition4))


  }
}