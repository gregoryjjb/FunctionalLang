package context

import value._
import expression._

class Environment extends collection.mutable.HashMap[Identifier, Value]
