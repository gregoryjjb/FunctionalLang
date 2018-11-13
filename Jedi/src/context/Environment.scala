package context

import value._
import expression._

class Environment(var parent: Environment = null) extends collection.mutable.HashMap[Identifier, Value] with Value {
  // used by closures to bind parameters to arguments
  def bulkPut(params: List[Identifier], args: List[Value]) {
    if (params.length != args.length) throw new TypeException("# arguments != #parameters")
    for(i <- 0 until params.length) this.put(params(i), args(i))
  }

  override def apply(name: Identifier): Value = {
    if (this.contains(name)) super.apply(name)
    else if (parent != null) parent.apply(name)
    else throw new UndefinedException(name)
  }

  override def contains(name: Identifier): Boolean => {

  }
}
