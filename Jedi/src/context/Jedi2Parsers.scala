/*
 * Gregory Brisebois 2018
 * CS152 Programming Paradigms
 * May contain content from Prof. John Pearce
 */

package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case Some(first ~ Nil) => List(first)
    case Some(first ~ rest) => first :: rest
    case None => List()
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~> params ~ expression ^^ {
    case p ~ e => Lambda(p, e)
  }

  def arrowLambda: Parser[Lambda] = params ~ "=>" ~ expression ^^ {
    case p ~ "=>" ~ e => Lambda(p, e)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case first ~ Nil => Block(List(first))
    case first ~ rest => Block(first :: rest)
  }

  def thunk: Parser[Expression] = "freeze(" ~> expression <~ ")" ^^ {
    case e => MakeThunk(e)
  }

  def text: Parser[Expression] = "delay(" ~> expression <~ ")" ^^ {
    case e => MakeText(e)
  }

  // override of term parser
  //override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
  override def term: Parser[Expression]  = lambda | thunk | text | arrowLambda | funCall | block | literal | "("~>expression<~")"
}
