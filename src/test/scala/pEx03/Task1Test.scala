package pEx03

import Exp.*

import org.scalatest.funsuite.AnyFunSuite

class Task1Test extends AnyFunSuite:
  test("Plain rename") {
    assertResult(
      Let("z", Num(5), Plus(Id("z"), Id("y")))
    )(
      {
        val exp = Let("x", Num(5), Plus(Id("x"), Id("y")))
        rename(exp, "x", "z")
      }
    )
  }

  test("ignore free variable") {
    assertResult(
      Let("x", Num(5), Plus(Id("x"), Id("y")))
    )(
      {
        val exp = Let("x", Num(5), Plus(Id("x"), Id("y")))
        rename(exp, "y", "z")
      }
    )
  }
