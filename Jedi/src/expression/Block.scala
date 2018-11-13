/*
 * Gregory Brisebois 2018
 * CS152 Programming Paradigms
 * May contain content from Prof. John Pearce
 */

package expression
import context._
import value._

case class Block(exps: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val blockEnv = new Environment(env)
    val results: List[Value] = exps.map(exp => exp.execute(blockEnv))
    results.last
  }
}
