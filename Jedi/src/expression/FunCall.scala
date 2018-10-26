package expression
import context._
import value._

class FunCall(name: Identifier, args: List[Value]) extends Expression {
  override def execute(env: Environment): Value = ???
}
