package hEx01

import org.scalatest.funsuite.AnyFunSuite

class Task4Test extends AnyFunSuite:

  val m1 = Map("A" -> 1, "B" -> 2)
  val m2 = Map("C" -> 3)
  val m3 = Map("A" -> 1, "B" -> 2, "C" -> 3)

  test("testMergeWithNonEmpty") {
    assertResult(
      m3
    )(
      merge(m1, m2)
    )
  }
