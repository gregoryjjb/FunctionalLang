package value

import context._
import expression._

case class Thunk(body: Expression,
                 defEnv: Environment
                ) extends Closure(List(), body, defEnv) {

  var result: Value = _

  def apply(): Value = {
    if(result == null) result = super.apply(List())
    result
  }
}
