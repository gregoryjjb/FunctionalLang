// DISCRETE DYNAMICAL SYSTEMS
// Gregory Brisebois

// PROBLEM 1
def controlLoop[S](
                    state: S,
                    cycle: Int,
                    halt: (S, Int) => Boolean,
                    update: (S, Int) => S
                  ): S = {
  if(halt(state, cycle)) state
  else controlLoop(update(state, cycle + 1), cycle + 1, halt, update)
}

// PROBLEM 2
controlLoop[Int](
  1,
  0,
  (state, cycle) => state > 100000,
  (state, cycle) => state * 2,
)

// PROBLEM 3
def solve(f: Double => Double) = {
  val delta = 1e-9
  def df(x: Double) = (f(x + delta) - f(x)) / delta
  controlLoop[Double](
    1.0,
    0,
    (state, _) => math.abs(f(state)) < delta,
    (state, _) => state - f(state) / df(state)
  )
}

// PROBLEM 4
def squareRoot(x: Double) = solve((n: Double) => n * n - x)

squareRoot(10)

// PROBLEM 5
def cubeRoot(x: Double) = solve((n: Double) => n * n * n - x)

cubeRoot(8)

// PROBLEM 6
def nthRoot(x: Double, n: Int) = solve((y: Double) => math.pow(y, n) - x)

nthRoot(16, 4)