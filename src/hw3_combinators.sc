
// IN CLASS

// PROBLEM 6
def multiThing[T](n: Int, start: T, callback: T => T): T = {
  var acc = start
  for(_ <- 0 until n) {
    acc = callback(acc)
  }
  acc
}

multiThing(5, "hi", (s: String) => { s.concat(" :) ") })
multiThing(7, 3, (i: Int) => { i + 1 })

def multiThingFunc[T](n: Int, start: T, callback: T => T): T = {
  if(n == 0) start
  else multiThingFunc(n - 1, callback(start), callback)
}

multiThingFunc(5, "hi", (s: String) => { s.concat(" :) ") })
multiThingFunc(7, 3, (i: Int) => { i + 1 })

// PROBLEM 7

