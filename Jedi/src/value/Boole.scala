package value

case class Boole(value: Boolean) extends Literal with Equals {

  // Operator overloads
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!this.value)

  // To String
  override def toString = if(value) "true" else "false"

  // Comparisons
  override def canEqual(other: Any) =  other.isInstanceOf[Boole]
  override def equals(other: Any): Boolean =
    other match {
      case other: Boole => this.canEqual(other) && (other.value == this.value)
      case _ => false
    }

  // Hash
  override def hashCode = this.toString.##
}
