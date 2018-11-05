package expression

import value._
import context._

case class Declaration(id: Identifier, exp: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    env.put(id, exp.execute(env))
    Notification.OK
  }
}
