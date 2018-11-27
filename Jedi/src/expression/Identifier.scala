package expression

import value._
import context._

case class Identifier(name: String) extends Expression {
  override def toString = name
  override def execute(env: Environment): Value = {
    val res = env.apply(this)
    res match {
      case thunk: Thunk => thunk.apply()
      case text: Text => text.apply(env)
      case _ => res
    }
  }
}
