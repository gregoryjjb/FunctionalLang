package expression
import context._
import value._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression) extends Expression{
  override def execute(env: Environment): Value = {
    val result: Value = condition.execute(env)
    if(!result.isInstanceOf[Boole]) return Notification.UNSPECIFIED //throw new TypeException("Conditional must resolve to a Boole")
    val b = result.asInstanceOf[Boole].value
    if(b) {
      consequent.execute(env)
    }
    else {
      alternative.execute(env)
    }
  }
}
