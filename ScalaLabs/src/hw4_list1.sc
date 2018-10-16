// Assignment 4, List Processing I
// Name: Gregory Brisebois

/****************************************
  * Problem 1: sum of odd cubes
  ***************************************/

// Iterative solution
def sumOddCubesIter(list: List[Int]): Int = {
  var sum = 0
  for (i <- list.indices) {
    if (list(i) % 2 != 0) sum = sum + math.pow(list(i), 3).toInt
  }
  sum
}

// Recursive solution
def sumOddCubesRecur(list: List[Int]): Int = {
  if(list == Nil) 0
  else if(list.head % 2 != 0) { math.pow(list.head, 3).toInt + sumOddCubesRecur(list.tail) }
  else sumOddCubesRecur(list.tail)
}

// Tail recursive solution
def sumOddCubesTail(list: List[Int]): Int = {
  def helper(l: List[Int], sum: Int): Int = {
    if(l == Nil) sum
    else if(l.head % 2 != 0) helper(l.tail, sum + math.pow(l.head, 3).toInt)
    else helper(l.tail, sum)
  }
  helper(list, 0)
}

// MPF solution
def sumOddCubesMFR(list: List[Int]): Int = {
  list
    .filter(el => el % 2 != 0)
    .map(el => math.pow(el, 3))
    .reduce(_ + _)
    .toInt
}

// Testing
val list1 = List(1, 2, 3)

sumOddCubesIter(list1)
sumOddCubesRecur(list1)
sumOddCubesTail(list1)
sumOddCubesMFR(list1)

/****************************************
  * Problem 2: sum of sums
  ***************************************/
// Iterative solution
def sumSumsIter(list: List[List[Int]]): Int = {
  var sum = 0
  for (i <- list.indices) {
    for (j <- list(i).indices) {
      sum = sum + list(i)(j)
    }
  }
  sum
}

// Recursive solution
def sumSumsRecur(list: List[List[Int]]): Int = {
  def helper(l: List[Int]): Int = {
    if (l == Nil) 0
    else l.head + helper(l.tail)
  }
  if (list == Nil) 0
  else helper(list.head) + sumSumsRecur(list.tail)
}

// tail recursive solution
def sumSumsTail(list: List[List[Int]]): Int = {
  def helper(l: List[List[Int]]) : Int = {
    def helper2(l2: List[Int]): Int = {
      if (l2 == Nil) 0
      else l2.head + helper2(l2.tail)
    }
    if (l == Nil) 0
    else helper2(l.head) + helper(l.tail)
  }
  helper(list)
}

// MPF solution
def sumSumsMFR(list: List[List[Int]]): Int = {
  list.map(l => l.reduce(_ + _)).reduce(_ + _)
}

// testing
val list2 = List(List(1, 2, 3), List(4, 5, 6))

sumSumsIter(list2)
sumSumsRecur(list2)
sumSumsTail(list2)
sumSumsMFR(list2)

/****************************************
  * Problem 3: depth
  ***************************************/
// solution
def depth(v: Any): Int = {
  v match {
    case Nil => 0
    case h::t => {
      val l = List.concat(List(h), t)
      l.map(el => depth(el)).max + 1
    }
    case _ => 0
  }
}

// testing
val list3 = List(List(List(1, 2, List(3))))

depth(list3)
depth(List())

depth(List(3,List(4)))

/****************************************
  * Problem 6: numPass
  ***************************************/
// Iterative solution
def numPassIter[T](list: List[T], f: T => Boolean): Int = {
  var n = 0
  for(i <- list.indices) {
    if(f(list(i))) n = n + 1
  }
  n
}

// Recursive solution
def numPassRecur[T](list: List[T], f: T => Boolean): Int = {
  if(list == Nil) 0
  else if(f(list.head)) 1 + numPassRecur(list.tail, f)
  else numPassRecur(list.tail, f)
}

// tail recursive solution
def numPassTail[T](list: List[T], f: T => Boolean): Int = {
  def helper(l: List[T], t: Int): Int = {
    if(l == Nil) t
    else if(f(l.head)) helper(l.tail, t + 1)
    else helper(l.tail, t)
  }
  helper(list, 0)
}

// MPF solution
def numPassMFR[T](list: List[T], f: T => Boolean): Int = {
  list.filter(el => f(el)).length
}

// testing
val list4 = List(0, 1, 2, 3, 4, 5)
def testF(n: Int): Boolean = n > 2

numPassIter(list4, testF)
numPassRecur(list4, testF)
numPassTail(list4, testF)
numPassMFR(list4, testF)

/****************************************
  * Problem 7: allPass
  ***************************************/
// Iterative solution
def allPassIter[T](list: List[T], f: T => Boolean): Boolean = {
  var p = true
  for(i <- list.indices) {
    p = p && f(list(i))
  }
  p
}

// Recursive solution
def allPassRecur[T](list: List[T], f: T => Boolean): Boolean = {
  if(list == Nil) true
  else f(list.head) && allPassRecur(list.tail, f)
}

// tail recursive solution
def allPassTail[T](list: List[T], f: T => Boolean): Boolean = {
  def helper(l: List[T], t: Boolean): Boolean = {
    if(l == Nil) t
    else helper(l.tail, t && f(l.head))
  }
  helper(list, true)
}

// MPF solution
def allPassMFR[T](list: List[T], f: T => Boolean): Boolean = {
  list.map(el => f(el)).reduce(_ && _)
}

// testing
val yesList = List(3, 4, 5)
val noList = List(0, 1, 2, 3, 4, 5)
// testF: return true if > 2

allPassIter(yesList, testF)
allPassIter(noList, testF)

allPassRecur(yesList, testF)
allPassRecur(noList, testF)

allPassTail(yesList, testF)
allPassTail(noList, testF)

allPassMFR(yesList, testF)
allPassMFR(noList, testF)

/****************************************
  * Problem 8: somePass
  ***************************************/
// Iterative solution
def anyPassIter[T](list: List[T], f: T => Boolean): Boolean = {
  var p = false
  for(i <- list.indices) {
    p = p || f(list(i))
  }
  p
}

// Recursive solution
def anyPassRecur[T](list: List[T], f: T => Boolean): Boolean = {
  if(list == Nil) false
  else f(list.head) || allPassRecur(list.tail, f)
}

// tail recursive solution
def anyPassTail[T](list: List[T], f: T => Boolean): Boolean = {
  def helper(l: List[T], t: Boolean): Boolean = {
    if(l == Nil) t
    else helper(l.tail, t || f(l.head))
  }
  helper(list, false)
}

// MPF solution
def anyPassMFR[T](list: List[T], f: T => Boolean): Boolean = {
  list.map(el => f(el)).reduce(_ || _)
}

// testing
val pass = List(3, 4, 5)
val noPass = List(0, 1, 2)
// testF: return true if > 2

anyPassIter(pass, testF)
anyPassIter(noPass, testF)

anyPassRecur(pass, testF)
anyPassRecur(noPass, testF)

anyPassTail(pass, testF)
anyPassTail(noPass, testF)

anyPassMFR(pass, testF)
anyPassMFR(noPass, testF)

/****************************************
  * Problem 10: isSorted
  ***************************************/
// solution
def isSorted(list: List[Int]): Boolean = {
  if(list.length < 2) true
  else {
    var b = true
    var prev = list(0)
    for(i <- 1 until list.length) {
      b = b && list(i) >= prev
      prev = list(i)
    }
    b
  }
}

// testing
val sorted = List(0, 1, 2, 3, 5, 7, 8)
val notSorted = List(0, 1, 5, 2, 1, 10)

isSorted(sorted)
isSorted(notSorted)

/****************************************
  * Problem 13: streams
  ***************************************/
// ones
def anotherOne(): Stream[Int] = 1 #:: anotherOne()
val ones = anotherOne()

ones take 10 foreach println

// nats
def nextInt(n: Int): Stream[Int] = n #:: nextInt(n + 1)
val ints = nextInt(0)

ints take 10 foreach println

// evens
def nextEven(n: Int): Stream[Int] = n #:: nextEven(n + 2)
val evens = nextEven(0)

evens take 10 foreach println

// squares
def nextSquare(n: Int): Stream[Int] = (n * n) #:: nextSquare(n + 1)
val squares = nextSquare(0)

squares take 10 foreach println