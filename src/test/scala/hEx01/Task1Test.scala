package hEx01

import org.scalatest.funsuite.AnyFunSuite
import hEx01.Nat.*

class Task1Test extends AnyFunSuite:
  
  val zero = Zero()
  val one = Succ(zero)
  val two = Succ(one)

  test("testAddZero") {
    assertResult(
      one
    )(
      add(zero, one)
    )
  }

  test("testAddTwo") {
    assertResult(
      two
    )(
      add(one, one)
    )
  }

  test("testMultZero") {
    assertResult(
      zero
    )(
      mult(zero, one)
    )
  }

  test("testMultOne") {
    assertResult(
      two
    )(
      mult(two, one)
    )
  }
