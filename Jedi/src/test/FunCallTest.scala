package test

import expression._
import context._
import value._


object FunCallTest extends App {
  val globalEnvironment = new Environment
  val operands = List(Integer(6), Integer(7))
  var exp = FunCall(Identifier("add"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("less"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("mul"), operands)
  println(exp.execute(globalEnvironment))

  // My test
  val cond = Boole(true) // FunCall(Identifier("equals"), operands)
  val ifTrue = FunCall(Identifier("add"), operands)
  val ifFalse = FunCall(Identifier("sub"), operands)
  val statement = Conditional(cond, ifTrue, ifFalse)
  println(statement.execute(globalEnvironment))
}