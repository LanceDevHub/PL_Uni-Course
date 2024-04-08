package hEx01

import org.scalatest.funsuite.AnyFunSuite


class Task2Test extends AnyFunSuite:

  test("testReverse") {
    assertResult(
      List(3,2,1)
    )(
      reverse(List(1, 2, 3))
    )
  }
