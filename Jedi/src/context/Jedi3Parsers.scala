package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
    case i ~ "=" ~ e => Assignment(i, e)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~> "(" ~> expression ~ ")" ~ expression ^^ {
    case cond ~ ")" ~ body => Iteration(cond, body)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case e => FunCall(Identifier("dereference"), List(e))
  }

  def break: Parser[Break] = "break" ^^ (_ => Break())

  override def expression: Parser[Expression] = break | declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | thunk | text | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}