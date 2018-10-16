// COMBINATORS
// Gregory Brisebois

// PROBLEM 1
def genericCompose[T](f: T => T, g: T => T): T => T = {
  x: T => f(g(x))
}

val addThree = genericCompose((x: Int) => x + 1, (x: Int) => x + 2)
addThree(5)

// PROBLEM 2
def selfIter[T](f: T => T, n: Int): T => T = {
  if(n == 0) (x: T) => x
  else genericCompose(f, selfIter(f, n - 1))
}

val addFive = selfIter((x: Int) => x + 1, 5)
addFive(0)

// PROBLEM 3
def countPass[T](arr: Array[T], pass: T => Boolean): Int = {
  arr.foldLeft(0)((a, b) => if(pass(b)) a + 1 else a)
}

var myArr = Array(1, 4, -5, -3, 2)
countPass(myArr, (e: Int) => e > 0)

// PROBLEM 4
def recur(base: Int, combiner: (Int, Int) => Int): Int => Int = {
  def f(n: Int): Int = {
    if (n == 0) base
    else combiner(n, f(n - 1))
  }
  f
}

recur(1, (a, b) => a * b)(5)

// PROBLEM 5
def deOptionize[T, U](f: T => Option[U]): T => U = {
  (t: T) => {
    val r = f(t)
    r match {
      case None => throw new Exception("Input error")
      case _ => r.get
    }
  }
}

def parseDigits(digits: String): Option[Int] =
  if (digits.matches("[0-9]*")) Some(digits.toInt) else None

deOptionize(parseDigits)("123")

// IN CLASS PROBLEMS

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
def test[T](pairs: Array[(T, T)], f: T => T): Int = {
  pairs.foldLeft(0)((acc, el) => if(f(el._1) != el._2) acc + 1 else acc)
}

test[Double](
  Array((0.0, 0.0), (1.0, 1.0), (2.0, 4.0), (3.0, 9.0), (4.0, 15.0), (5.0, 25.0)),
  n => n * n
)