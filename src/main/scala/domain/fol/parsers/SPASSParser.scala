package domain.fol.parsers

import ast._
import core.containers.{ClauseStorage, CNFClauseStore}
import helpers.Logging
import scala.util.parsing.combinator.syntactical._

/**
 * User: nowi
 * Date: 27.11.2009
 * Time: 17:59:38
 */
object SPASSParser extends StandardTokenParsers with Logging {
  lexical.delimiters ++= List("(", ")", ").", "[", "]", ".", ". ", ",", ", ", ";", "{", "}")
  lexical.reserved += ("", "exists", "forall", "and", "or", "not", "implies", "implied", "equiv", "clause", "cnf",
          "dnf", "listofclauses", "true", "false", "axioms", "conjectures", "listofformulae",
          "endoflist", "predicate", "subsort", "sort", "freely", "generatedby", "listofdeclarations",
          "listofsymbols", "sorts", "predicates", "functions", "listofdescriptions", "satisfiable", "unsatisfiable",
          "unknown", "endproblem", "beginproblem", "formula", "name", "author", "version", "logic", "status", "description", "date")


  def problem = "beginproblem" ~ "(" ~ ident ~ ")." ~ description ~ ". " ~ logicalpart ~ rep(settings) ~ "endproblem" ~ "."


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

  def predicate: Parser[Term] = ident ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
    case ident ~ "(" ~ terms ~ ")" if (ident.toString.charAt(0).isLowerCase) => Function(ident.toString, terms)
    case ident ~ "(" ~ terms ~ ")" if (ident.toString.charAt(0).isUpperCase) => Predicate(ident.toString, terms)
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


  def clauselist: Parser[ClauseStorage] = "listofclauses" ~ "(" ~ origintype ~ "," ~ clausetype ~ ")." ~ rep(clause) ~ "endoflist" ~ ". " ^^ {
    case "listofclauses" ~ "(" ~ co ~ "," ~ ct ~ ")." ~ clauses ~ "endoflist" ~ ". " => CNFClauseStore(Set[FOLClause]() ++ clauses)
  }

  //  def clause : Parser[StandardClause]= "clause" ~ "(" ~ opt(cnfclause | dnfclause) ~ opt("," ~ label) ~ ")."  ^^ {
  //    case "clause" ~ "(" ~ c ~ "," ~ l ~ ")." => StandardClause(c.args)
  //    case "clause" ~ c  => StandardClause(c.args)
  //    case "clause"   => StandardClause()
  //  }

  def clause: Parser[StandardClause] = "clause" ~ "(" ~ (cnfclause | dnfclause) ~ "," ~ label ~ ")." ^^ {
    case "clause" ~ "(" ~ c ~ "," ~ l ~ ")." => StandardClause(c)
  }

  def clausetype = "cnf" | "dnf"

  def cnfclause: Parser[Sentence] = universalCNFClause | orConnective

  def universalCNFClause: Parser[UniversalQuantifer] = "forall" ~ "(" ~ variableList ~ "," ~ orConnective ~ ")" ^^ {
    case "forall" ~ "(" ~ vars ~ "," ~ connective ~ ")" => {
      val x = UniversalQuantifer(connective, vars)
      log.debug("Crated universal cnf clause %s", x)
      x

    }

  }

  def dnfclause: Parser[Sentence] = existentialDNFClause | andConnective

  def existentialDNFClause: Parser[ExistentialQuantifer] = "exists" ~ "(" ~ variableList ~ "," ~ andConnective ~ ")" ^^ {
    case "exists" ~ "(" ~ vars ~ "," ~ connective ~ ")" => ExistentialQuantifer(connective, vars)
  }


  def arity = numericLit

  def prooflist = "listofproof" ~ "endoflist."

  def settings = "listofgeneralsettings" ~ "endoflist."


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

    input.replace("_", "").replace("'", "").replace("*", "'").replace(", ", ",").replace("\n", " ")

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

}