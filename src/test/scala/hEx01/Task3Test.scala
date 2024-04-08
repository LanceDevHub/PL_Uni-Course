package hEx01

import org.scalatest.funsuite.AnyFunSuite

class Task3Test extends AnyFunSuite:
  test("flatten") {
    assertResult(
      List(1, 2, 3, 4, 5, 6)
    )(
      flatten(List(List(1), List(2, 3, 4), List(5, 6)))
    )
  }