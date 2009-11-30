package domain.fol.parsers

/**
 * User: nowi
 * Date: 28.09.2009
 * Time: 14:28:33
 */

import ast._
import scala.util.parsing.combinator.syntactical._

object FOLDsl extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", ",")
  lexical.reserved += ("exists", "forall", "some", "and", "or", "not", "iff", "then", "if")

  def sentence: Parser[Sentence] = atomicsentence | connective | quantifier | negation

  def negation: Parser[Sentence] = "not" ~ "(" ~> sentence <~ ")" ^^ {case s => Negation(s)}

  def atomicsentence: Parser[AtomicSentence] = relation | termEquality

  def termEquality: Parser[TermEquality] = term ~ "iff" ~ term ^^ {case t1 ~ "iff" ~ t2 => TermEquality(t1, t2)}

  def term: Parser[Term] = relation | constant | variable

  def connective: Parser[Sentence] = "(" ~> sentence ~ ("then" | "or" | "and" | "iff") ~ sentence <~ ")" ^^ {
    case s1 ~ "then" ~ s2 => ImplicationConnective(s1, s2)
    case s1 ~ "or" ~ s2 => OrConnective(List(s1, s2))
    case s1 ~ "and" ~ s2 => AndConnective(List(s1, s2))
    case s1 ~ "iff" ~ s2 => EqualityConnective(s1, s2)
  }

  def quantifier: Parser[Sentence] = ("forall" | "exists") ~ repsep(variable, ",") ~ sentence ^^ {
    case "forall" ~ vars ~ s => UniversalQuantifer(s, vars)
    case "exists" ~ vars ~ s => ExistentialQuantifer(s, vars)

  }

  def relation: Parser[Predicate] = ident ~ terms ^^ {case i ~ terms => Predicate(i, terms)}

  def terms: Parser[List[Term]] =
    "(" ~> repsep(term, ",") <~ ")" ^^ {(terms: List[Term]) => terms}


  def variable: Parser[Variable] = ident ^^ {case v => Variable(v)}

  def constant: Parser[Constant] = stringLit ^^ {case s => Constant(s)}


  def parse(dsl: String) =
    {
      val tokens = new lexical.Scanner(dsl)
      phrase(sentence)(tokens) match {
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

  def parse2(dsl: String): Option[Sentence] =
    {
      val tokens = new lexical.Scanner(dsl)
      phrase(sentence)(tokens) match {
        case Success(tree, _) => {
          println(tree)
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }


    }

}