package hEx04

import org.scalatest.funsuite.AnyFunSuite

import hEx04.*

class Task1Test extends AnyFunSuite:

  test("testLengthOfString") {
    assertResult(List(1, 3, 5, 1, 2, 3))(lengthOfString(List("a", "bcd", "efghi", "j", "kl", "mno")))
  }

  test("testMap2") {
    assertResult(List("A", "B", "CDE", "FGH"))(map((x: String) => x.toUpperCase, List("a", "B", "cDe", "FgH")))
  }

  test("testExistsTrue1") {
    assertResult(true)(exists(List(false, false, false, true)))
  }
