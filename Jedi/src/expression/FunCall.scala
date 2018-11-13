package expression

import context._
import value._

case class FunCall(name: Identifier, args: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {

    val ops = args.map(_.execute(env))

    if(env.contains(name) && env.apply(name).isInstanceOf[Closure]) {
      // The function is in the environment
    }
    else {
      alu.execute(name, args.map(arg => arg.execute(env)))
    }
  }
}
