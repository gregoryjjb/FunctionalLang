def inc(n: Int):Int = n + 1
def dec(n: Int):Int = n - 1
def isZero(n: Int):Boolean = n == 0

// PROBLEM 1
def add(n: Int, m: Int): Int = {
  if(isZero(m)) n
  else inc(addTail(n, dec(m)))
}

add(0, 5)
add(7, 2)

def addTail(n: Int, m: Int): Int = {
  if(isZero(m)) n
  else add(inc(n), dec(m))
}

addTail(0, 5)
addTail(7, 2)

// PROBLEM 2
def mul(n: Int, m: Int): Int = {
  if(isZero(m) || isZero(n)) 0
  else add(n, mul(n, dec(m)))
}

mul(0, 3)
mul(4, 5)

def mulTail(n: Int, m: Int): Int = {
  if(isZero(m) || isZero(n)) 0
  else mul(addTail(n, n), dec(m))
}

mul(0, 3)
mul(4, 5)

// PROBLEM 3
def exp2(m: Int): Int = {
  if(isZero(m)) 1
  else mul(2, exp2(dec(m)))
}

exp2(0)
exp2(2)
exp2(8)

def exp2Tail(m: Int): Int = {
  def helper(e: Int, n: Int): Int = {
    if(isZero(e)) n
    else helper(dec(e), mulTail(n, 2))
  }

  helper(m, 1)
}

exp2Tail(0)
exp2Tail(2)
exp2Tail(8)

// PROBLEM 4
def hyperExp(n: Int): Int = {
  if(isZero(n)) 1
  else exp2(hyperExp(dec(n)))
}

hyperExp(0)
hyperExp(1)
hyperExp(2)
hyperExp(3)

def hyperExpTail(n: Int): Int = {
  def helper(e: Int, n: Int): Int = {
    if(isZero(e)) n
    else helper(dec(e), exp2Tail(n))
  }

  helper(n, 1)
}

hyperExp(0)
hyperExp(1)
hyperExp(2)
hyperExp(3)

// PROBLEM 5


// PROBLEM 9
def fib(n: Int): Int = {
  if(isZero(n)) 0
  else if(isZero(dec(n))) 1
  else add(fib(dec(n)), fib(dec(dec(n))))
}

fib(3)
fib(8)

def fibTail(n: Int): Int = {
  def helper(a: Int, b: Int, n: Int): Int = {
    if(isZero(n)) a
    else if(isZero(dec(n))) b
    else helper(b, addTail(a, b), dec(n))
  }

  helper(0, 1, n)
}

fibTail(3)
fibTail(8)