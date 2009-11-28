package domain.fol.parsers

import scala.util.parsing.combinator.syntactical._

/**
 * User: nowi
 * Date: 27.11.2009
 * Time: 17:59:38
 */
object SPASSParser extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", ").", "[", "]", ".", ". ", ",", ", ", ";")
  lexical.reserved += ("", "exists", "forall", "and", "or", "not", "implies", "implied", "equiv", "clause", "cnf",
          "dnf", "listofclauses", "true", "false", "axioms", "conjectures", "listofformulae",
          "endoflist", "predicate", "subsort", "sort", "freely", "generatedby", "listofdeclarations",
          "listofsymbols", "sorts", "predicates", "functions", "listofdescriptions", "satisfiable", "unsatisfiable",
          "unknown", "endproblem", "beginproblem", "formula")


  def problem = "beginproblem" ~ "(" ~> ident <~ ")." ~ description ~ ". " ~ logicalpart ~ rep(settings) ~ "endproblem" ~ "."


  def description = "listofdescriptions" ~ ". " ~ "endoflist"

  def logstate = "satisfiable" | "unsatisfiable" | "unknown"


  def logicalpart = symbollist ~ rep(formulalist) ~ rep(clauselist) ~ rep(prooflist)

  def symbollist = "listofsymbols" ~ ". " ~ functions ~ ". " ~ predicates ~ ". " ~ "endoflist" ~ ". "

  def declarationlist = "listofdeclarations." ~ rep(declaration) ~ "endoflist"

  def declaration = subsortdec1 | termdec1 | preddec1 | gendec1

  def gendec1 = "sort" ~ sortsym ~ opt("freely") ~ "generatedby" ~ funclist ~ "."

  def funclist = "[" ~ repsep(funsym, ",") ~ "]."

  def subsortdec1 = "subsort(" ~ sortsym ~ "," ~ sortsym ~ ")."

  def termdec1 = "forall(" ~ termlist ~ "," ~ term ~ ")."

  def preddec1 = "predicate(" ~ predsym ~ rep1sep(sortsym, ",") ~ ")."

  def sortsym = ident

  def predsym = ident

  def funsym = ident

  def formulalist = "listofformulae" ~ "(" ~ origintype ~ ")." ~ rep(formula) ~ "endoflist" ~ ". "


  def formula = "formula" ~ "(" ~ term ~ "," ~ label ~ ")." | "formula" ~ "(" ~ term ~ ")." | "formula" ~ "(" ~ ")."

  def origintype = "axioms" | "conjectures"

  def label = ident


  def term: Parser[Any] = quantsym ~ "(" ~ termlist ~ "," ~ term ~ ")" | symbol ~ "(" ~ repsep(term, ",") ~ ")" | symbol

  def termlist = "[" ~ repsep(term, ",") ~ "]"

  def quantsym = "forall" | "exists"


  def symbol = "equal" | "true" | "false" | "or" | "and" | "not" | "implies" | "implied" |
          "equiv" | ident


  def clauselist = "listofclauses(" ~ origintype ~ "," ~ clausetype ~ ")." ~ rep(clause) ~ "endoflist"

  def clause = "clause(" ~ opt(cnfclause | dnfclause) ~ opt("," ~ label) ~ ")."

  def clausetype = "cnf" | "dnf"

  def cnfclause = "forall(" ~ termlist ~ "," ~ cnfclausebody ~ ")" | cnfclausebody

  def dnfclause = "exists(" ~ termlist ~ "," ~ dnfclausebody ~ ")" | dnfclausebody

  def cnfclausebody = "or(" ~ repsep(term, ",") ~ ")"

  def dnfclausebody = "and(" ~ repsep(term, ",") ~ ")"


  def arity = numericLit

  def prooflist = "listofproof" ~ "endoflist."

  def settings = "listofgeneralsettings" ~ "endoflist."


  def functions = "functions" ~ "[" ~ funs ~ "]"

  def funs: Parser[List[Any]] = repsep(function, ", ")

  def function = "(" ~ funsym ~ "," ~ arity ~ ")"

  def predicates = "predicates" ~ "[" ~ repsep(predicate, ",") ~ "]"

  def predicate = predsym | "(" ~ predsym ~ "," ~ arity ~ ")"

  def sorts = "sorts[" ~ repsep(sort, ",") ~ "]."

  def sort = sortsym


  def parse(dsl: String) =
    {
      val tokens = new lexical.Scanner(dsl)
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