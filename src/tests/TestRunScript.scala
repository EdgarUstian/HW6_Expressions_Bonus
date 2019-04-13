package tests

import org.scalatest._
import expressions.Expressions._

class TestRunScript extends FunSuite {
  //Operator Functions
  val pow: (Double, Double) => Double = (a: Double, b: Double) => Math.pow(a, b)
  val mul: (Double, Double) => Double = (a: Double, b: Double) => a * b
  val div: (Double, Double) => Double = (a: Double, b: Double) => a / b
  val add: (Double, Double) => Double = (a: Double, b: Double) => a + b
  val sub: (Double, Double) => Double = (a: Double, b: Double) => a - b

  //Operator Table
  val operatorTable: Map[String, (Double, Double) => Double] = Map(
    "^" -> pow,
    "*" -> mul,
    "/" -> div,
    "+" -> add,
    "-" -> sub
  )

  //Operation Order
  val operationOrder = List(
    List("^"),       // highest precedence
    List("*", "/"),  // medium precedence
    List("+", "-")   // lowest precedence
  )

  //Boolean Functions
  val and: (Boolean, Boolean) => Boolean = (a: Boolean, b: Boolean) => a && b
  val or: (Boolean, Boolean) => Boolean = (a: Boolean, b: Boolean) => a || b
  val xor: (Boolean, Boolean) => Boolean = (a: Boolean, b: Boolean) => (a || b) && !(a && b)
  val implies: (Boolean, Boolean) => Boolean = (a: Boolean, b: Boolean) => !(a && !b)
  val iff: (Boolean, Boolean) => Boolean = (a: Boolean, b: Boolean) => (a && b) || (!a && !b)

  //Boolean Table
  val booleanTable: Map[String, (Boolean, Boolean) => Boolean] = Map(
    "&&" -> and,
    "||" -> or,
    "xor" -> xor,
    "->" -> implies,
    "<>" -> iff
  )

  //Boolean Order
  val booleanOrder = List(
    List("&&"),       // highest precedence
    List("||", "xor"),  // medium precedence
    List("->", "<>")   // lowest precedence
  )

  test("Test Arithmetic Script"){
    assert(runScript[Double]("data/aritScript.txt", (s: String) => s.toDouble, operatorTable, operationOrder) == 6.0)
  }
  test("Test Boolean Script"){
    assert(runScript[Boolean]("data/boolScript.txt", (s: String) => s.toBoolean, booleanTable, booleanOrder))
  }
}
