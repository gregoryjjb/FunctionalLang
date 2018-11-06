package context

import scala.util.parsing.combinator._
import expression._
import value._

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  // Expression
  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  // Declaration
  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  // Conditional
  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  // Disjunction
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con :: more)
  }

  // Conjunction ::= equality ~ ("&&" ~ equality)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case eq ~ Nil  => eq
    case eq ~ more => Conjunction(eq :: more)
  }

  // Equality ::= inequality ~ ("==" ~ inequality)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case con ~ Nil  => con
    case con ~ more => FunCall(Identifier("equals"), con :: more)
  }

  // Inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case sum ~ None => sum
    case sum ~ Some("<" ~ sum2)  => FunCall(Identifier("less"), List(sum, sum2))
    case sum ~ Some(">" ~ sum2)  => FunCall(Identifier("more"), List(sum, sum2))
    case sum ~ Some("!=" ~ sum2) => FunCall(Identifier("unequals"), List(sum, sum2))
  }

  // Negate(exp) = 0 - exp
  private def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Integer(0)
    FunCall(sub, List(zero, exp))
  }

  // Sum ::= product ~ ("+" | "-") ~ product)*
  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product ^^ {
    case "+"~s=>s
    case "-"~s=> negate(s)
  })^^{
    case p~Nil=> p
    case p~rest=>FunCall(Identifier("add"), p :: rest)
  }

  // Product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
    case t ~ blah => parseProduct(t, blah)
  }

  // generates left-to-right calls to mul and div:
  private  def parseProduct(t: Expression, terms: List[~[String, Expression]]): Expression = {
    terms match {
      case Nil => t
      case ~("*", t1)::more => parseProduct(FunCall(Identifier("mul"), List(t, t1)), more)
      case ~("/", t1)::more => parseProduct(FunCall(Identifier("div"), List(t, t1)), more)
    }
  }

  // Term
  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"

  // Literal
  def literal: Parser[Expression] = boole | real | integer | chars | identifier

  // Chars ::= any characters bracketed by quotes
  def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ (characters => Chars(characters.substring(1, characters.length - 1)))

  // Integer ::= 0|(\+|-)?[1-9][0-9]*
  def integer: Parser[Integer] = """0|(\+|-)?[1-9][0-9]*""".r ^^ (s => Integer(s.toInt))

  // Real ::= (\+|-)?[0-9]+\.[0-9]+
  def real: Parser[Real] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^ (s => Real(s.toDouble))

  // Boole ::= true | false
  def boole: Parser[Boole] = """true|false""".r ^^ (s => Boole(s.toBoolean))

  // Identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ (s => Identifier(s))

  // FunCall ::= identifier ~ operands
  def funCall: Parser[FunCall] = identifier ~ operands ^^ {
    case i~o => FunCall(i, o)
  }

  // Operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~ opt(expression ~ rep("," ~ expression)) ~ ")" ^^ {
    case _~Some(exp ~ Nil)~_ => List(exp)
    case _~Some(exp ~ rest)~_ => exp :: rest.map(f => f._2)
    case _ => List()
  }
}
