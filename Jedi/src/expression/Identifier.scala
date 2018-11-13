package expression

import value._
import context._

case class Identifier(name: String) extends Expression {
  override def toString = name
  override def execute(env: Environment): Value = {
    env.apply(this)
  }
}
