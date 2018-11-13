/*
 * Gregory Brisebois 2018
 * CS152 Programming Paradigms
 * May contain content from Prof. John Pearce
 */

package value
import context._
import expression._

case class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Expression]): Value = {
    if(args.length != params.length) throw new JediException("Wrong number of arguments")

    val closureEnv = new Environment(defEnv)
    closureEnv.bulkPut(params, args.map(e => e.execute(defEnv)))
    body.execute(closureEnv)
  }
}
