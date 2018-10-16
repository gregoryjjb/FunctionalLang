//Assignment 4, List Processing II
// Name: Gregory Brisebois

import scala.collection.mutable.ListBuffer

/****************************************
  * Problem 1: score processing
  ***************************************/
// avg
def avg(scores: List[Double]): Double = scores.sum / scores.length

// avgAvg
def avgAvg(scores: List[List[Double]]): List[Double] = scores.map(el => avg(el))

// passing
def passing(scores: List[List[Double]]): List[Int] = {
  var p = List[Int]()
  val avgs = avgAvg(scores)
  for(i <- avgs.indices) {
    if(avgs(i) >= 70) p = i :: p
  }
  p
}

// sumSums
def sumSums(scores: List[List[Double]]): Double = scores.map(l => l.sum).sum

// testing
val cs152 = List(List(93.0, 89.0, 90.0), List(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0))

avgAvg(cs152)

passing(cs152)

sumSums(cs152)

/****************************************
  * Problem 2: spellCheck
  ***************************************/
// solution
def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {
  val b = new ListBuffer[String]
  for(i <- doc.indices) {
    val s = doc(i)
    if(dictionary.find(el => el == s).isEmpty) b += s
  }
  b.toList
}

// testing
val doc = List("Here", "are", "some", "words")
val dict = List("Here", "some")

spellCheck(doc, dict)

/****************************************
  * Problem 3: spellCheck using map, filter, etc.
  ***************************************/
// solution
def spellCheckMFR(doc: List[String], dictionary: List[String]): List[String] = {
  doc.filter(s => dictionary.find(d => d == s).isEmpty)
}

// testing
spellCheckMFR(doc, dict)

/****************************************
  * Problem 4: polynomials
  ***************************************/
// evalMono
def evalMono(mono: (Double, Double), x: Double): Double = mono._1 * math.pow(x, mono._2)

// evalPoly
def evalPoly(poly: List[(Double, Double)], x: Double): Double = {
  poly.map(p => evalMono(p, x)).sum
}

// testing
evalMono((3, 2), 2)
evalPoly(List((3.0, 2.0), (-5.0, 0.0)), 4)