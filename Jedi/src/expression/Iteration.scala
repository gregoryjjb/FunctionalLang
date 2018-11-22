package expression

import context._
import value._

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var result: Value = Notification.OK
    var condVal = condition.execute(env)

    while(condVal.isInstanceOf[Boole] && condVal.asInstanceOf[Boole].value) {
      result = body.execute(env)
      condVal = condition.execute(env)
    }

    if(!condVal.isInstanceOf[Boole]) throw new TypeException("While loops need Booles")

    result
  }
}
