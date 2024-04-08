package hEx04

import org.scalatest.funsuite.AnyFunSuite


class Task2Test extends AnyFunSuite:

  test("testNil") {
    assertResult(Nil)(filter((x: Int) => x > 4, Nil))
  }

  test("testLength2") {
    assertResult(
      List("bc", "de", "gh")
    )(
      filter((x: String) => x.length == 2, List("a", "bc", "de", "f", "gh", "i"))
    )
  }
