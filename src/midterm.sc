//////////////////////////////
// Gregory Brisebois
// CS152 Midterm Exam

//////////////////////////////
// PROBLEM 1

def addPositives(l: List[Option[Int]]): Int = {
  l.filter(el => el.isDefined && el.get > 0).map(el => el.get).sum
}

addPositives(List(Some(1), Some(2), None, Some(-3), Some(4)))
addPositives(List(Some(-1), Some(-2), Some(-3)))

//////////////////////////////
// PROBLEM 2

//////////////
// PART A

trait Command {
  def execute(): Unit
}

// Provided by exam
object accumulator {
  var accum: Int = 0
  def execute(program: Command*) = {
    accum = 0
    for(cmmd <- program) cmmd.execute() // updates accum
    accum
  }
}

def Set(n: Int = 0) = new Command {
  override def execute(): Unit = {
    accumulator.accum = n
  }
}

def Add(n: Int) = new Command {
  override def execute(): Unit = {
    accumulator.accum = accumulator.accum + n
  }
}

def Mul(n: Int) = new Command {
  override def execute(): Unit = {
    accumulator.accum = accumulator.accum * n
  }
}

accumulator.execute(Set(5), Add(3))
accumulator.execute(Set(), Add(2), Mul(3))
accumulator.execute(Set(-5), Mul(3), Add(1))

//////////////
// PART B

def Iter(cmd: Command, n: Int) = new Command {
  override def execute(): Unit = {
    for(_ <- 0 until n) {
      cmd.execute()
    }
  }
}

// Tail recursive version, for fun
def TailIter(cmd: Command, n: Int) = new Command {
  override def execute(): Unit = {
    def helper(i: Int): Unit = {
      if(i <= 0) return
      else {
        cmd.execute()
        helper(i - 1)
      }
    }
    helper(n)
  }
}

accumulator.execute(Set(), Add(5), Iter(Add(1), 5))
accumulator.execute(Set(1), Iter(Mul(2), 6))
accumulator.execute(Set(1), TailIter(Mul(2), 6))

//////////////////////////////
// PROBLEM 3

def applyFilters[T](vals: List[T], tests: List[T=>Boolean]): List[T] = {
  if(tests == Nil) vals
  else applyFilters(vals.filter(tests.head), tests.tail)
}

val myTests = List((x: Int) => x % 2 == 0, (x: Int) => x > 10, (x: Int) => x < 50)

applyFilters(List(7, 8, 12, 15, 20, 52), myTests)
applyFilters(List(-5, 22, 23, 48, 49, 50), myTests)