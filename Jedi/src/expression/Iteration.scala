package expression

import context._
import value._

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var result: Value = Notification.OK
    var condVal = condition.execute(env)
    var break = false

    while(!break && condVal.isInstanceOf[Boole] && condVal.asInstanceOf[Boole].value) {
      try {
        result = body.execute(env)
        condVal = condition.execute(env)
      }
      catch {
        case be: BreakException => break = true
        case e: Exception => throw e
      }
    }

    if(!condVal.isInstanceOf[Boole]) throw new TypeException("While loops need Booles")

    result
  }
}
