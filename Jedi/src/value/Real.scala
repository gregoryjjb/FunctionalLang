package value

case class Real(value: Double) extends Literal with Ordered[Real] with Equals {

  // Operator overloads
  def +(other: Real) = Real(this.value + other.value)
  def -(other: Real) = Real(this.value - other.value)
  def /(other: Real) = Real(this.value / other.value)
  def *(other: Real) = Real(this.value * other.value)
  def unary_- = Real(value * -1)

  // To String
  override def toString = value.toString

  // Comparisons
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Real]
  def compare(other: Real): Int = {
    if (this.value < other.value) -1
    else if (other.value < this.value) 1
    else 0
  }
  override def equals(other: Any): Boolean =
    other match {
      case other: Real => this.canEqual(other) && (other.value == this.value)
      case _ => false
    }

  // Hash
  override def hashCode = this.toString.##
}
