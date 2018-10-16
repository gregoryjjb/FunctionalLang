package value

import expression._
import context._

trait Literal extends Value with Expression {
  def execute(env: Environment) = this
}
