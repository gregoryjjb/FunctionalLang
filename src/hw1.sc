///////////////////////
// SEQUENCE CONTROL

// PROBLEM 1
def tax(income: Double): Double = income * (income match {
  case x if x < 0 => throw new IllegalArgumentException("Income cannot be negative")
  case x if x < 20000 => 0.0
  case x if x < 30000 => 0.05
  case x if x < 40000 => 0.11
  case x if x < 60000 => 0.23
  case x if x < 100000 => 0.32
  case _ => 0.50
})

tax(100.0)
tax(45000.0)
tax(200000.0)

// PROBLEM 2
def drawRectangle(x: Int, y: Int) = {
  for(i <- 1 to y) {
    for(j <- 1 to x) {
      print("*")
    }
    println()
  }
}

drawRectangle(6, 2)

// PROBLEM 3
def printSums(n: Int, m: Int) = {
  for {
    i <- 0 to n - 1
    j <- 0 to m - 1
    if n > 0
    if m > 0
  } println(i + " + " + j + " = " + (i + j))
}

printSums(1, 4)

// PROBLEM 4
def mystery() = {
  var break = false
  var continue = false

  for(i <- 0 to 99) {
    continue = false

    if(i % 3 == 0) continue = true
    if(i == 10) break = true

    if(!break && !continue) println("i = " + i)
  }

  println("done")
}

mystery()

// PROBLEM 5
def root(x: Double): Option[Double] = if (x < 0) None else Some(math.sqrt(x))
def below10(x: Double): Option[Double] = if (x < 10) Some(x) else None

def pureRoot(x: Option[Double]): Option[Double] = {
  x match {
    case None => None
    case Some(y) => root(y)
  }
}

def pureBelow10(x: Option[Double]): Option[Double] = {
  x match {
    case None => None
    case Some(y) => below10(y)
  }
}

def below10Root(x: Option[Double]): Option[Double] = pureBelow10(pureRoot(x))

below10Root(Some(4))
below10Root(Some(9))
below10Root(Some(100))
below10Root(Some(-100))

///////////////////////
// MATHEMATICS

import scala.math._

// PROBLEM 1
def solve(a: Double, b: Double, c: Double): Option[(Double, Double)] = {
  if (b * b - 4.0 * a * c < 0) None
  else Some(
    (-b + sqrt(b * b - 4.0 * a * c)) / (2 * a),
    (-b - sqrt(b * b - 4.0 * a * c)) / (2 * a),
  )
}

solve(2, -2, -4)
solve(1, 0, 1)
solve(1, 0, -1)

// PROBLEM 2
def dist(p1: (Double, Double), p2: (Double, Double)): Double = {
  sqrt(pow(p2._1 - p1._1, 2) + pow(p2._2 - p1._2, 2))
}

dist((1, 1), (0, 0))
dist((3, 0), (0, 0))

// PROBLEM 3
def dot(a: (Double, Double, Double), b: (Double, Double, Double)): Double = {
  a._1 * b._1 + a._2 * b._2 + a._3 * b._3
}

dot((2.0, 3, 4), (2, 2.0, 2))

// PROBLEM 4
def force(m1: Double, m2: Double, r: Double): Double = {
  (6.67408E-11 * m1 * m2) / r
}

force(100000, 150000, 15)

// PROBLEM 5
def mean(vals: Array[Double]): Double = {
  vals.sum / vals.length
}

mean(Array(2.0, 3, 4, 5))

def stdDev(vals: Array[Double]): Double = {
  val m = mean(vals)
  sqrt(mean(vals.map(f => pow(f - m, 2))))
}

stdDev(Array(2.0, 3, 4, 5))

// PROBLEM 6
def isPrime(n: Int): Boolean = n match {
  case x if x < 0 => throw new IllegalArgumentException("Cannot check primacy of negative number")
  case 0 => false
  case 1 => false
  case 2 => true
  case _ => {
    var prime = true
    for(
      i <- 2 until n
      if prime
    ) {
      if(n % i == 0) prime = false
    }
    prime
  }
}

isPrime(2)
isPrime(4)
isPrime(199)
isPrime(200)

// PROBLEM 7
// Phi function

// PROBLEM 8
def rollDice(): (Int, Int) = {
  def oneSix(): Int = (floor(random() * 6) + 1).toInt
  (oneSix(), oneSix())
}

rollDice()
rollDice()
rollDice()
rollDice()

///////////////////////
// STRING PROCESSING

import scala.util.Random
import scala.util.matching.Regex

// PROBLEM 1
def isPal(s: String): Boolean = {
  val half = floor(s.length / 2).toInt
  var allMatch = true
  for {
    i <- 0 to half
    j = s.length - i - 1
  } if(s.charAt(i) != s.charAt(j)) allMatch = false
  allMatch
}

isPal("racecar")
isPal("yessey")
isPal("$$.%.$$")
isPal("nope")

// PROBLEM 2
def isPal2(s: String): Boolean = isPal(s.replaceAll("\\s+|[.,\\/!#$%*?\\^\\*;:{}=\\-_`~()'\"]", "").toLowerCase())

isPal2("A man, a plan, a canal, Panama!")
isPal2("Race car")
isPal2("Race kar")

// PROBLEM 3
def mkPal(s: String): String = s.concat(s.reverse)

mkPal("mars")
mkPal("!337h@x")

// PROBLEM 4
def mkWord(size: Int = 5): String = {
  var s = ""
  for(i <- 1 to size) s = s.concat(Random.alphanumeric.filter(_.isLetter).head.toLower.toString())
  s
}

mkWord()
mkWord(10)

// PROBLEM 5
def mkSentence(size: Int = 10): String = {
  var a = Array[String]()
  for(_ <- 1 to size) {
    a = a :+ mkWord((random() * 10 + 1).toInt)
  }
  a.mkString(" ").concat(".").capitalize
}

mkSentence()
mkSentence(2)

// PROBLEM 6
def shuffle(s: String): String = s.drop(s.length / 2).concat(s.take(s.length / 2))

shuffle("abcxyz")

// PROBLEM 7
def countSubstrings(m: String, s: String): Int = m.r.findAllIn(s).length

countSubstrings("is", "Mississippi")
countSubstrings("ham", "Ham ham hamham haam")

// BONUS: RECURSIVE PROBLEM 7
def countSubstringsRecursive(m: String, s: String): Int = {
  val next = s.replaceFirst(m, "")
  if(next.length == s.length) 0
  else countSubstringsRecursive(m, next) + 1
}

countSubstringsRecursive("is", "Mississippi")
countSubstringsRecursive("ham", "Ham ham hamham haam")

// PROBLEM 8
def evaluate(e: String): Double = {

  val strs = e.replaceAll("\\s+", "").split("[+]")

  // If I uncomment this the function stops working completely
  //if(strs.length <= 1) {
  //  throw new IllegalArgumentException("Missing operator")
  //}

  strs.map(s => s.toDouble).sum
}

evaluate("1 + 2")
evaluate("5 + 3.2")