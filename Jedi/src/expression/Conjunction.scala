package expression

import context._
import value._

// &&
case class Conjunction(exps: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {

    var result = true

    for(exp <- exps if result) {
      val b = exp.execute(env)
      if(!b.isInstanceOf[Boole])
        throw new TypeException("Arguments to && must be Boole")
      result = b.asInstanceOf[Boole].value && result
    }

    Boole(result)
  }
}
