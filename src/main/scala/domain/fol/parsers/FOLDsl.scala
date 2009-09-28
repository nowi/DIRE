package domain.fol.parsers

/**
 * User: nowi
 * Date: 28.09.2009
 * Time: 14:28:33
 */

import scala.util.parsing.combinator.syntactical._

object FOLDsl extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", ",")
  lexical.reserved += ("exists", "forall", "some", "and", "or", "not", "iff", "then", "if")

  def sentence: Parser[Any] = {println("Sentence"); atomicsentence | "(" ~> sentence ~ connective ~ sentence <~ ")" | quantifier ~ repsep(variable, ",") ~ sentence | negation}

  def negation: Parser[Any] = {println("negation"); "not" ~ "(" ~> sentence <~ ")"}

  def atomicsentence: Parser[Any] = relation | term

  def term: Parser[Any] = {println("Term"); relation | constant | variable}

  def connective: Parser[Any] = {println("connective"); ("then" | "or" | "and" | "iff")}

  def quantifier: Parser[Any] = {println("Quantifier"); ("forall" | "exists")}

  //  def function: Parser[Any] = { println("Function");"f" ~ ident }

  //  def predicate: Parser[Any] = { println("Predicate");"p" ~ ident }

  def relation: Parser[Any] = {println("Relation"); ident ~ "(" ~> repsep(term, ",") <~ ")"}


  def variable: Parser[Any] = {println("Variable"); ident}

  def constant: Parser[Any] = {println("Constant"); stringLit}


  def parse(dsl: String) =
    {
      val tokens = new lexical.Scanner(dsl)
      phrase(sentence)(tokens) match {
        case Success(tree, _) => {
          println(tree)
        }
        case e: NoSuccess => Console.err.println(e)
      }


    }

}