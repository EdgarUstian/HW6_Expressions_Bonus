package tests

import expressions.Expressions._
import org.scalatest._

class TestBoolean extends FunSuite {

  test("Test Boolean Expression"){

    //Test True
    val boolExpression1 = "(true -> false) <> (false || false)"
    assert(evaluateBoolean(boolExpression1))
    val boolExpression2 = "true || false"
    assert(evaluateBoolean(boolExpression2))

    //Test False
    val boolExpression3 = "true || false && false -> false xor true && false"
    assert(!evaluateBoolean(boolExpression3))



  }
}
