package domain.fol.parsers

import ast._
import core.containers.{ClauseStorage, CNFClauseStore}
import helpers.Logging
import java.io.File
import scala.util.parsing.combinator.syntactical._

/**
 * User: nowi
 * Date: 27.11.2009
 * Time: 17:59:38
 */
object SPASSIntermediateFormatParser extends StandardTokenParsers with Logging {
  lexical.delimiters ++= List("(", ")", ").", "[", "]", ".", ". ", ",", ", ", ";", "{", "}", "->", "+")
  lexical.reserved += ("", "exists", "forall", "and", "or", "not", "implies", "implied", "equiv", "clause", "cnf",
          "dnf", "listofclauses", "true", "false", "axioms", "conjectures", "listofformulae",
          "endoflist", "predicate", "subsort", "sort", "freely", "generatedby", "listofdeclarations",
          "listofsymbols", "sorts", "predicates", "functions", "listofdescriptions", "satisfiable", "unsatisfiable",
          "unknown", "endproblem", "beginproblem", "formula", "name", "author", "version", "logic", "status", "description", "date", "->", "listofsettings")


  def problem = "beginproblem" ~ "(" ~ ident ~ ")." ~ description ~ ". " ~ logicalpart ~ "endproblem" ~ "."


  def description = "listofdescriptions" ~ ". " ~ name ~ author ~ opt(version) ~ opt(logic) ~ status ~ desc ~ opt(date) ~ "endoflist"


  def name = "name" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def author = "author" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def version = "version" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def logic = "logic" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def status = "status" ~ "(" ~ logstate ~ ")."

  def desc = "description" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def date = "date" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."


  def logstate = "satisfiable" | "unsatisfiable" | "unknown"


  def logicalpart: Parser[Any] = symbollist ~ rep(formulalist) ~ rep(clauselist) ~ rep(prooflist)

  def symbollist = "listofsymbols" ~ ". " ~ opt(functions ~ ". ") ~ opt(predicates ~ ". ") ~ "endoflist" ~ ". "

  //  def declarationlist = "listofdeclarations." ~ rep(declaration) ~ "endoflist"
  def declarationlist = "listofdeclarations." ~ "endoflist"

  //  def declaration = subsortdec1 | termdec1 | preddec1 | gendec1
  //
  //  def gendec1 = "sort" ~ sortsym ~ opt("freely") ~ "generatedby" ~ funclist ~ "."
  //
  //  def funclist = "[" ~ repsep(funsym, ",") ~ "]."
  //
  //  def subsortdec1 = "subsort(" ~ sortsym ~ "," ~ sortsym ~ ")."
  //
  //  def termdec1 = "forall(" ~ termlist ~ "," ~ term ~ ")."
  //
  //  def preddec1 = "predicate(" ~ predsym ~ rep1sep(sortsym, ",") ~ ")."
  //
  //  def sortsym = ident

  def predsym = ident

  def funsym = ident

  def formulalist = "listofformulae" ~ "(" ~ origintype ~ ")." ~ rep(formula) ~ "endoflist" ~ ". "


  def formula = "formula" ~ "(" ~ term ~ "," ~ label ~ ")." | "formula" ~ "(" ~ term ~ ")." | "formula" ~ "(" ~ ")."

  def origintype = "axioms" | "conjectures"

  def label = numericLit


  def term: Parser[Sentence] = quantTerm | negation | connective | orConnective | andConnective | predicate | variable

  def predicate: Parser[Term] = ident ~ "(" ~ repsep(term, ",") ~ ")" ~ opt("+") ^^ {
    case ident ~ "(" ~ terms ~ ")" ~ None if (ident.toString.charAt(0).isLowerCase) => Function(ident.toString, terms)
    case ident ~ "(" ~ terms ~ ")" ~ None if (ident.toString.charAt(0).isUpperCase) => Predicate(ident.toString, terms)
    case ident ~ "(" ~ terms ~ ")" ~ Some(plus) if (ident.toString.charAt(0).isLowerCase) => Function(ident.toString, terms)
    case ident ~ "(" ~ terms ~ ")" ~ Some(plus) if (ident.toString.charAt(0).isUpperCase) => Predicate(ident.toString, terms)
    case _ => error("Should not be here")
  }

  def variable: Parser[Variable] = ident ^^ {
    case v => Variable(v.toString)
  }

  def connective: Parser[Connective] = ("equal" | "equiv" | "implies" | "implied") ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
    case "equal" ~ "(" ~ terms ~ ")" => EqualityConnective(terms(0), terms(1))
    case "equiv" ~ "(" ~ terms ~ ")" => EqualityConnective(terms(0), terms(1))
    case "implies" ~ "(" ~ terms ~ ")" => ImplicationConnective(terms(0), terms(1))
    case "implied" ~ "(" ~ terms ~ ")" => ImplicationConnective(terms(1), terms(0))
  }

  def negation = "not" ~ "(" ~ term ~ ")" ^^ {
    case "not" ~ "(" ~ t ~ ")" => Negation(t)
  }

  def andConnective: Parser[Connective] = "and" ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
    case "and" ~ "(" ~ terms ~ ")" => AndConnective(terms)
  }

  def orConnective: Parser[Connective] = "or" ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
    case "or" ~ "(" ~ terms ~ ")" => OrConnective(terms)
  }


  def quantTerm: Parser[Quantifier] = ("exists" | "forall") ~ "(" ~ variableList ~ "," ~ term ~ ")" ^^ {
    case "forall" ~ "(" ~ vars ~ "," ~ t ~ ")" => UniversalQuantifer(t, vars)
    case "exists" ~ "(" ~ vars ~ "," ~ t ~ ")" => ExistentialQuantifer(t, vars)
  }

  def variableList: Parser[List[Variable]] = "[" ~ repsep(variable, ",") ~ "]" ^^ {
    case "[" ~ vars ~ "]" => {
      log.debug("Created variable list : %s", vars)
      vars
    }
  }


  def clauselist: Parser[List[FOLClause]] = "listofclauses" ~ "(" ~ origintype ~ "," ~ clausetype ~ ")." ~ rep(clause) ~ "endoflist" ~ ". " ^^ {
    case "listofclauses" ~ "(" ~ co ~ "," ~ ct ~ ")." ~ clauses ~ "endoflist" ~ ". " => clauses
  }


  def clause: Parser[ALCDClause] = "clause" ~ "(" ~ (cnfclause | dnfclause) ~ "," ~ label ~ ")." ^^ {
    case "clause" ~ "(" ~ c ~ "," ~ l ~ ")." => ALCDClause(c.args: _*)
  }


  def sharedClauselist: Parser[List[FOLClause]] = "listofclauses" ~ "(" ~ origintype ~ "," ~ clausetype ~ ")." ~ rep(sharedClause) ~ "endoflist" ~ ". " ^^ {
    case "listofclauses" ~ "(" ~ co ~ "," ~ ct ~ ")." ~ clauses ~ "endoflist" ~ ". " => clauses
  }


  def sharedClause: Parser[ALCDClause] = "clause" ~ "(" ~ (cnfclause | dnfclause) ~ "," ~ label ~ ")." ^^ {
    case "clause" ~ "(" ~ c ~ "," ~ l ~ ")." => ALCDClause(c.args.map(_.shared) : _*)
  }

  def clausetype = "cnf" | "dnf"



  //clause( eins || zwei  ->  drei  ).
  //'||' kannst du ignorieren, das ist um die Sorts (erste Liste) vom Rest zu trennen. 'drei' sind positive literale die anderen sind negativ, das heisst bei den Literalen vor dem Pfeil muß ein 'not' davor wenn man es ins 'or' schreibt.
  //also:
  //clause(or(NEWATOMIC3(U), Size(U))).
  //clause(or(not(N(U)),not(P(U)))).






  def cnfclause: Parser[Sentence] = universalCNFClause


  def universalCNFClause: Parser[OrConnective] = opt(rep(predicate)) ~ "->" ~ opt(rep(predicate)) ^^ {
    case Some(negativeLiterals) ~ "->" ~ Some(positiveLiterals) => {
      val x = OrConnective(negativeLiterals.map({Negation(_)})) ++ OrConnective(positiveLiterals)
      log.debug("Crated universal cnf clause %s", x)
      x

    }

    case None ~ "->" ~ Some(positiveLiterals) => {
      val x = OrConnective(positiveLiterals)
      log.debug("Crated universal cnf clause %s", x)
      x

    }

    case Some(negativeLiterals) ~ "->" ~ None => {
      val x = OrConnective(negativeLiterals.map({Negation(_)}))
      log.debug("Crated universal cnf clause %s", x)
      x

    }

  }


  def negativeLiterals = repsep(term, " ") ^^ {
    case terms => {
      terms.map({Negation(_)})
    }

  }

  def positiveLiterals = repsep(term, " ") ^^ {
    case terms => {
      terms
    }

  }


  def dnfclause: Parser[Sentence] = existentialDNFClause | andConnective

  def existentialDNFClause: Parser[ExistentialQuantifer] = "exists" ~ "(" ~ variableList ~ "," ~ andConnective ~ ")" ^^ {
    case "exists" ~ "(" ~ vars ~ "," ~ connective ~ ")" => ExistentialQuantifer(connective, vars)
  }


  def arity = numericLit

  def prooflist = "listofproof" ~ "endoflist."

  def settings = "listofsettings(SPASS). {'" ~ "'} endoflist"


  def functions = "functions" ~ "[" ~ funs ~ "]"

  def funs: Parser[List[Any]] = repsep(functiondef, ",")

  def functiondef = "(" ~ funsym ~ "," ~ arity ~ ")"

  def predicates = "predicates" ~ "[" ~ repsep(predicatedef, ",") ~ "]"

  def predicatedef = predsym | "(" ~ predsym ~ "," ~ arity ~ ")"

  //  def sorts = "sorts[" ~ repsep(sort, ",") ~ "]."
  //
  //  def sort = sortsym


  def convertInput(input: String): String = {
    // "_" --> ""
    // remove "'"
    // "*" --> "'"
    // remove line breaks

    input.replace("_", "").replace("'", "").replace("*", "'").replace(", ", ",").replace("\n", " ").replace("\t", "").replace("||", "")

  }


  def parse(dsl: String) =
    {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(problem)(tokens) match {
        case Success(tree, _) => {
          println(tree)
          true

        }
        case e: NoSuccess => {
          Console.err.println(e)
          false
        }
      }


    }


  def parseClauseStore(dsl: String): Option[List[FOLClause]] =
    {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(clauselist)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }


    }

  def parseClauseStoreShared(dsl: String): Option[List[FOLClause]] =
    {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(sharedClauselist)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }


    }


  def parseFromFile(file: File): List[FOLClause] = {
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines // parse
    val clauses = SPASSIntermediateFormatParser.parseClauseStore(text)

    clauses match {
      case None => throw new IllegalStateException("Could not load clauses from file")
      case Some(clauses) => {
        clauses
      }
    }

  }

  def parseSharedFromFile(file: File): List[FOLClause] = {
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines // parse
    val clauses = SPASSIntermediateFormatParser.parseClauseStoreShared(text)

    clauses match {
      case None => throw new IllegalStateException("Could not load clauses from file")
      case Some(clauses) => {
        clauses
      }
    }

  }

}