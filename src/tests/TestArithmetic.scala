package tests

import org.scalatest._
import expressions.Expressions._

class TestArithmetic extends FunSuite {

  val EPSILON: Double = 0.000001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test Arithmetic Expression"){

    //Eliminating Spaces
    assert(evaluateArithmetic("     2.5  +  2.5     ")=="5".toDouble)

    //Testing Multiple Operations
    assert(evaluateArithmetic(" (12-4) - (8+9/3) ")=="-3".toDouble)
    assert(evaluateArithmetic("10-(8/12.0*6)/2-1")=="7".toDouble)

    //Test Exponentiation
    assert(evaluateArithmetic("2^2")=="4".toDouble)
    assert(evaluateArithmetic("3^(((5*5^3)+3)-(5*5^3)")=="27".toDouble)

  }
}
