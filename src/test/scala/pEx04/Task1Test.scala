package pEx04

import org.scalatest.funsuite.AnyFunSuite

import pEx04.*

class Task1Test extends AnyFunSuite:

  test("testPow21") {
    assertResult(List(1, 4, 9, 16))(pow2(List(1, 2, 3, 4)))
  }

  test("testForallFalse1") {
    assertResult(false)(forall(List(false, true, true, true)))
  }
