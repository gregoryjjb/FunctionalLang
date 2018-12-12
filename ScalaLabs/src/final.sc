case class Entry(val hour: Int, val temp: Double) {
  override def toString = "[" + hour + ":00 temp = " + temp + " degs]"
}

def logProcessor(log: List[Entry]): List[Entry] = {
  log
    .filter(el => el.hour < 17)
    .map(el => Entry(el.hour, (el.temp * 1.8) + 32))
}

val readings = List(Entry(6, 25), Entry(10, 28), Entry(12, 32), Entry(16, 30), Entry(18, 26), Entry(22, 19))

logProcessor(readings)