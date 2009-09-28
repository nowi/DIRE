package domain.fol.parsers

/**
 * User: nowi
 * Date: 28.09.2009
 * Time: 14:28:33
 */

import scala.util.parsing.combinator.syntactical._

object FOLDsl extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", ",")
  lexical.reserved += ("exist", "forall", "some", "and", "or", "not", "<->", "->", "<-")

  def sentence = atomicsentence | "(" ~> sentence ~ connective ~ sentence <~ ")" | quantifier ~ repsep(variable, ",") ~ sentence | "not" ~ sentence

  def atomicsentence = predicate ~ "(" ~> repsep(term, ",") <~ ")" | term

  def term = function ~ "(" ~> repsep(term, ",") <~ ")" | constant | variable

  def connective = ("->" | "or" | "and" | "<->")

  def quantifier = ("forall" | "exists")

  def function = "fun" ~ ident

  def variable = stringLit

  def constant = ident
}