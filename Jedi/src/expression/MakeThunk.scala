package expression

import context._
import value._

case class MakeThunk(body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = Thunk(body, env)
}
