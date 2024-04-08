package hEx08.Task2

import Exp.*
import hEx08.Task2
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class Task2Test extends AnyFunSuite:

  implicit def stringToId(name: String): Exp = Id(name)
  implicit def intToNumC(n: Int): Num = Num(n)

  def runProg(expr: Exp): Value = interp(expr, Map(), Map())._1

  test("box-like storage") {
    assertResult(
      Value.Int(1)
    )(
      runProg(Let("array", NewArray(1), Seq(WriteArray("array", 0, 1), ReadArray("array", 0))))
    )
  }

  test("array storage") {
    assertResult(
      Value.Int(6)
    )(
      runProg(Let("array", NewArray(3),
        Seq(
          Seq(
            Seq(
              WriteArray("array", 0, Plus(1, 2)),
              WriteArray("array", 1, 2)),
            WriteArray("array", 2, 1)),
          Plus(Plus(ReadArray("array", 1), ReadArray("array", 2)), ReadArray("array", 0)))))
    )
  }

  test("set multiple times") {
    assertResult(
      Value.Int(1)
    )(
      runProg(Let("array", NewArray(3),
        Seq(
          Seq(
            Seq(
              WriteArray("array", 0, 3),
              WriteArray("array", 0, 2)),
            WriteArray("array", 0, 1)),
          ReadArray("array", 0))))
    )
  }

  test("complex index/size expressions") {
    assertResult(
      Value.Int(6)
    )(
      runProg(Let("f", Fun(List("x"), Plus("x", 1)), Let("array", NewArray(App("f", List(2))),
        Seq(
          Seq(
            Seq(
              WriteArray("array", App("f", List(-1)), Plus(1, 2)),
              WriteArray("array", Plus(0, 1), 2)),
            WriteArray("array", 2, 1)),
          Plus(Plus(ReadArray("array", App("f", List(0))), ReadArray("array", Plus(1,1))), ReadArray("array", 0))))))
    )
  }

  test("invalid size") {
    assertThrows[RuntimeException] {
      runProg(Let("array", NewArray(Fun(List("x"), "x")),
        Seq(
          WriteArray("array", 0, 1),
          ReadArray("array", 0))))
    }
  }

  test("zero size") {
    assertThrows[RuntimeException] {
      runProg(Let("array", NewArray(0), Num(0)))
    }
  }

  test("out-of-bounds get-index") {
    assertThrows[RuntimeException] {
      runProg(Seq(NewArray(5), Let("array", NewArray(3), Seq(NewArray(5), Seq(WriteArray("array", 0, 1), ReadArray("array", 3))))))
    }
  }
