package expression

import context._
import value._

case class Assignment(varName: Identifier, assign: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val variable = varName.execute(env)
    variable match {
      case v: Variable =>
        v.value = assign.execute(env)
        Notification.DONE
      case _ => throw new TypeException("Can't assign non-variable")
    }
  }
}
