/*
 * Gregory Brisebois 2018
 * CS152 Programming Paradigms
 * May contain content from Prof. John Pearce
 */

package expression
import context._
import value._

case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    new Closure(params, body, env)
  }
}
