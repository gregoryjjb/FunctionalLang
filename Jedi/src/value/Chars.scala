package value

case class Chars(value: String) extends Literal with Ordered[Chars] with Equals {

  def substring(start: Integer, end: Integer): Chars = Chars(value.substring(start.value, end.value))

  // Operator overloads
  def +(other: Chars) = Chars(this.value + other.value)

  // To String
  override def toString = value

  // Comparisons
  def compare(other: Chars): Int = {
    if (this.value < other.value) -1
    else if (other.value < this.value) 1
    else 0
  }
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Chars]
  override def equals(other: Any): Boolean =
    other match {
      case other: Chars => this.canEqual(other) && (other.value == this.value)
      case _ => false
    }

  // Hash
  override def hashCode = this.toString.##
}
