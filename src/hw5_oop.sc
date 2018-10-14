////////////////////////////////////////
// Gregory Brisebois
// CS152 Homework 5
// Object-Oriented Programming

/////////////////////////////
// PROBLEM 1

abstract class Character(val name: String) {
  var health = 100
}

class Dragon(name: String) extends Character(name) {
  def attack(victim: Knight) = {
    println(name + " is flaming " + victim.name)
    val dmg: Double = Math.floor(Math.random() * health)
    victim.health = Math.max(0, victim.health - dmg.toInt)
  }
}

class Knight(name: String) extends Character(name) {
  def attack(victim: Dragon) = {
    println(name + " is stabbing " + victim.name)
    val dmg: Double = Math.floor(Math.random() * health)
    victim.health = Math.max(0, victim.health - dmg.toInt)
  }
}

object Dungeon {
  val random = new scala.util.Random(System.nanoTime())

  def main(args: Array[String]): Unit = {
    val puff = new Dragon("Puff")
    val thor = new Knight("Thor")
    while (puff.health > 0 && thor.health > 0) {
      thor.attack(puff)
      puff.attack(thor)
      println("Thor's health is " + thor.health)
      println("Puff's health is " + puff.health)
    }
    println("Thor's health is " + thor.health)
    println("Puff's health is " + puff.health)
  }
}

Dungeon.main(Array())

/////////////////////////////
// PROBLEM 2

import scala.collection.mutable.ArrayBuffer

class Queue[T](initial: Array[T]) {
  private val buffer = ArrayBuffer.empty[T]
  buffer ++= initial

  def enqueue(el: T): Unit = {
    buffer += el
  }

  def dequeue(): T = {
    if(buffer.isEmpty) throw new Exception("Queue is empty")
    else {
      val el: T = buffer.head
      buffer -= buffer(0)
      el
    }
  }

  def isEmpty: Boolean = buffer.isEmpty

  override def toString: String = {
    var str = "Queue("
    for(i <- buffer.indices) {
      str += buffer(i)
      if(i < buffer.length - 1) str += ", "
    }
    str + ")"
  }
}

object Queue {
  def apply[T](arr: Array[T]): Queue[T] = new Queue(arr)
}

var waitingList = Queue(Array("Sam", "Chris", "Mark", "Luke", "Jessica"))
println(waitingList)
while(!waitingList.isEmpty) {
  println(waitingList.dequeue())
}

/////////////////////////////
// PROBLEM 3

class Description(val text: String, val price: Int, val supplier: String) {
  override def toString: String = text
}

object Description {
  def apply(text: String, price: Int, supplier: String): Description = new Description(text, price, supplier)
}

class Item(val description: Description, val id: Int) {
  override def toString: String = id + " - " + description.text
}

object Item {
  var nextId: Int = 0
  def apply[T](description: Description): Item = {
    val item = new Item(description, nextId)
    nextId += 1
    item
  }
}

object Indus {
  var inventory = Map.empty[Description, Int]

  def rightPad(s: String, n: Int): String = {
    var padded = s;
    while(padded.length < n) padded = padded + " "
    padded
  }

  def printInventory(): Unit = {
    println("Description            Price   Supplier     Stock")
    inventory.foreach(el => {
      print(rightPad(el._1.text, 23))
      print(rightPad(el._1.price.toString, 8))
      print(rightPad(el._1.supplier, 13))
      print(el._2)
      println()
    })
  }

  def main(args: Array[String]): Unit = {
    inventory += Description("The Matrix DVD", 1550, "DVD World") -> 5
    inventory += Description("The Terminator DVD", 1325, "DVD World") -> 3
    inventory += Description("Iron Man DVD", 1800, "DVD Planet") -> 2

    printInventory()
  }
}

Indus.main(Array())

/////////////////////////////
// PROBLEM 4

abstract class Expression {
  def execute: Double
}

class Sum(val op1: Expression, val op2: Expression) extends Expression {
  override def execute: Double = op1.execute + op2.execute

  override def toString: String = "(+ " + op1 + " " + op2 + ")"
}
object Sum {
  def apply(op1: Expression, op2: Expression) = new Sum(op1, op2)
}

class Product(val op1: Expression, val op2: Expression) extends Expression {
  override def execute: Double = op1.execute * op2.execute

  override def toString: String = "(* " + op1 + " " + op2 + ")"
}
object Product {
  def apply(op1: Expression, op2: Expression) = new Product(op1, op2)
}

class Number(val value: Double) extends Expression {
  override def execute: Double = value

  override def toString: String = value.toString
}
object Number {
  def apply(value: Double) = new Number(value)
}

var exp: Expression = Sum(Number(42), Product(Number(3.14), Number(2.71)))
println("the value of " + exp + " = " + exp.execute)
exp = Product(Number(2), Product(Number(3), Number(5)))
println("the value of " + exp + " = " + exp.execute)

/////////////////////////////
// PROBLEM 5

trait IThermometer {
  // = avg degrees Farenheit
  def getMeanTemperature(cities: List[String]): Double
}

class CelsiusTherm {
  // = degrees Celsius
  def computeTemp(city: String) = 50 * math.random // fake temperature for now
}

class ThermAdapter extends CelsiusTherm with IThermometer {
  override def getMeanTemperature(cities: List[String]): Double = {
    cities.map(computeTemp).sum / cities.length
  }
}

object WeatherStation extends App {
  val thermometer: IThermometer = new ThermAdapter
  val avgTemp = thermometer.getMeanTemperature(List("LA", "SF", "SLC", "Rio"))
  println("avg temp = " + avgTemp)
}

WeatherStation.main(Array())