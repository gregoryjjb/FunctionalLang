case class Note(
                 amplitude: Double,
                 frequency: Double,
                 duration: Double = 1.0
               )

val symphony1 = List(
  Note(3, 30),
  Note(3.1, 40, .25),
  Note(3.2, -10, .5),
  Note(2.1, 5, .75),
  Note(3.9, -2)
)

//def duration(score: List[Note]): Double = { 0 }

////////////////
// Max amp

def maxAmpRecur(score: List[Note]): Double = {
  if(score == Nil) 0
  else math.max(score.head.amplitude, maxAmpRecur(score.tail))
}

maxAmpRecur(symphony1)

def maxAmpTail(score: List[Note]) = {
  def helper(result: Double, unseen: List[Note]): Double =
    if (unseen == Nil) result
    else helper(math.max(result, unseen.head.amplitude), unseen.tail)
  helper(0.0, score)
}

maxAmpTail(symphony1)

def maxAmpMFR(score: List[Note]): Double = {
  score.map(n => n.amplitude).reduce((a, b) => math.max(a, b))
}

///////////////
// Filter noise

def noise(note: Note) = note.frequency < 0

def filterNoise(score: List[Note]): List[Note] = {
  var newScore: List[Note] = Nil
  for(note <- score) if (!noise(note)) newScore = newScore :+ note
  newScore
}

filterNoise(symphony1)

def filterNoiseRecur(score: List[Note]): List[Note] = {
  if(score == Nil) List()
  else if(!noise(score.head))
    score.head :: filterNoiseRecur(score.tail)
  else
    filterNoiseRecur(score.tail)
}

filterNoiseRecur(symphony1)

def filterNoiseTail(score: List[Note]): List[Note] = {
  def helper(acc: List[Note], remaining: List[Note]): List[Note] = {
    if(remaining == Nil) acc
    else if(!noise(remaining.head))
      helper(acc :+ remaining.head, remaining.tail)
    else
      helper(acc, remaining.tail)
  }
  helper(List(), score)
}

filterNoiseTail(symphony1)

def filterNoiseMFR(score: List[Note]): List[Note] = {
  score.filter(n => !noise(n))
}

filterNoiseMFR(symphony1)

//////////////////
// Amplify Note

def amplifyNote(note: Note, amt: Double = 2.0): Note = {
  Note(amt * note.amplitude, note.frequency, note.duration)
}

def amplify(score: List[Note]): List[Note] = {
  var result: List[Note] = Nil
  for(note <- score) result = result :+ amplifyNote(note)
  result
}

def amplifyRecur(score: List[Note]): List[Note] = {
  if (score == Nil) Nil
  else amplifyNote(score.head) :: amplifyRecur(score.tail)
}

amplifyRecur(symphony1)

def amplifyMFR(score: List[Note]): List[Note] = {
  score.map(n => amplifyNote(n))
}

amplifyMFR(symphony1)