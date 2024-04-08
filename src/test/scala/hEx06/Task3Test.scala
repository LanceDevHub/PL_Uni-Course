package hEx06

import org.scalatest.funsuite.AnyFunSuite


class Task3Test extends AnyFunSuite:
  test("testAsBoolTrue") {
    assertResult(true)(asBoolean(tru))
  }

  test("testNotFalse") {
    assertResult(true)(asBoolean(not(fls)))
  }

  test("testAnd1") {
    assertResult(true)(asBoolean(and(tru, tru)))
  }

  test("testOr1") {
    assertResult(true)(asBoolean(or(tru, fls)))
  }

  test("testNested1") {
    assertResult(false)(asBoolean(and(or(tru, fls), not(and(tru, tru)))))
  }

  test("testTest1") {
    assertResult("Ok")(testBool(not(tru), "NotOk", "Ok"))
  }
