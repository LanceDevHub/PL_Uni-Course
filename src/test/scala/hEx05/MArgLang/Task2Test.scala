package hEx05.MArgLang

import hEx05.MArgLang.*
import hEx05.MArgLang.Exp.*
import org.scalatest.funsuite.AnyFunSuite

class Task2Test extends AnyFunSuite:
//TODO: out-comment test
/*
  val twoArgClosure = Value.Closure(List("x", "y"), Plus(Id("x"), Id("y")), Map())
  val twoArgClosure2 = Value.Closure(List("x", "y"), Mult(Id("x"), Id("y")), Map())
  val threeArgClosure = Value.Closure(List("x", "y", "z"), Plus(Id("x"), Mult(Id("y"), Id("z"))), Map())
  val g = Value.Closure(List("n"), Mult(Id("n"), Num(1)), Map())
  val env = Map("f" -> Value.Closure(List("n"), App(Id("g"), List(Plus(Id("n"), Num(5)))), Map("g" -> g)),
    "g" -> Value.Closure(List("n"), Mult(Id("n"), Num(1)), Map()))

  test("testSingleArgApp") {
    assertResult(
      Value.Int(10)
    )(
      interp(
        App(Id("f"), List(Num(5))),
        env)
    )
  }

  test("testNoArgsApp") {
    assertResult(
      Value.Int(2)
    )(
      interp(
        App(Id("c"), List()),
        Map("c" -> Value.Closure(List(), Num(2), Map())))
    )
  }

  test("testTwoArgsApp") {
    assertResult(
      Value.Int(3)
    )(
      interp(
        App(Id("f"), List(Num(1), Num(2))),
        Map("f" -> twoArgClosure))
    )
  }

  test("testWrongNumberOfArgsApp") {
    assertThrows[Exception] {
      interp(
        App(Id("f"), List(Num(1))),
        Map("f" -> twoArgClosure))
    }
  }


 */