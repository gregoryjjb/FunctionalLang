package value

import context._
import expression._

case class Text(body: Expression) extends Value {
  def apply(env: Environment): Value = {
    body.execute(env)
  }
}
