package expression

import value._
import context._

case class Identifier(val name: String) extends Expression {
  override def toString = name
  override def execute(env: Environment): Value = {
    env(name)
  }
}
