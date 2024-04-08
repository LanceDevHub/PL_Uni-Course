package pEx06

import org.scalatest.funsuite.AnyFunSuite


class Task1Test extends AnyFunSuite:

  test("toInt") {
    assertResult(toInt(zero))(0)
  }

  test("succ") {
    assertResult(toInt(succ(zero)))(1)
    assertResult(toInt(succ(one)))(2)
  }

  test("numbers") {
    assertResult(toInt(one))(1)
    assertResult(toInt(two))(2)
    assertResult(toInt(three))(3)
  }

  test("add") {
    assertResult(toInt(add(one)(two)))(3)
    assertResult(toInt(add(two)(two)))(4)
    assertResult(toInt(add(two)(three)))(5)
  }



