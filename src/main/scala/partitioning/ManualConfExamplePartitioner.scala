package partitioning


import core.containers.{CNFClauseStore, ClauseStorage}
import domain.fol.ast.{NegativeFOLLiteral, PositiveFOLLiteral, FOLNode, FOLClause}
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Logging
import java.io.File

/**
 * User: nowi
 * Date: 21.01.2010
 * Time: 19:48:14
 */

class ManualConfExamplePartitioner extends ClauseStoragePartitioning with Logging {
  // parser
  // TODO externalize this
  val parser = SPASSIntermediateFormatParser

  override def partition(clauses: ClauseStorage) = {
    // load the main ontology

    val module0 = SPASSIntermediateFormatParser.parseSharedFromFile(new File("input/conf/conf0.dire"))
    val module1 = SPASSIntermediateFormatParser.parseSharedFromFile(new File("input/conf/conf1.dire"))
    val module2 = SPASSIntermediateFormatParser.parseSharedFromFile(new File("input/conf/conf2.dire"))
    val module3 = SPASSIntermediateFormatParser.parseSharedFromFile(new File("input/conf/conf3.dire"))
    val module4 = SPASSIntermediateFormatParser.parseSharedFromFile(new File("input/conf/conf4.dire"))


    // assert that each module is hosting correctly

    require(module0.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top
        case NegativeFOLLiteral(negL) => negL.top
      }).substring(0, 2) == "O0"})}))


    require(module1.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top
        case NegativeFOLLiteral(negL) => negL.top
      }).substring(0, 2) == "O1"})}))

    require(module2.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top
        case NegativeFOLLiteral(negL) => negL.top
      }).substring(0, 2) == "O2"})}))

    require(module3.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top
        case NegativeFOLLiteral(negL) => negL.top
      }).substring(0, 2) == "O3"})}))


    require(module4.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top
        case NegativeFOLLiteral(negL) => negL.top
      }).substring(0, 2) == "O4"})}))


    List(
      CNFClauseStore(module0),
      CNFClauseStore(module1),
      CNFClauseStore(module2),
      CNFClauseStore(module3),
      CNFClauseStore(module4)
      )


  }
}

class ManualConfExampleMerger extends ClauseStoragePartitioning with Logging {
  // parser
  // TODO externalize this
  val parser = SPASSIntermediateFormatParser

  override def partition(clauses: ClauseStorage) = {
    // load the main ontology

    val module0 = SPASSIntermediateFormatParser.parseFromFile(new File("input/conf/merged.dire")).removeDuplicates

    // merge those modules into one

    List(CNFClauseStore(module0))


  }
}

class ManualConfExampleMergerShared extends ClauseStoragePartitioning with Logging {
  // parser
  // TODO externalize this
  val parser = SPASSIntermediateFormatParser

  override def partition(clauses: ClauseStorage) = {
    // load the main ontology

    val module0 = SPASSIntermediateFormatParser.parseSharedFromFile(new File("input/conf/merged.dire")).removeDuplicates

    // merge those modules into one

    List(CNFClauseStore(module0))


  }
}