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
