package hEx08.Task1

import Exp.*
import hEx08.Task1
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class Task1Test extends AnyFunSuite:

  implicit def intToExp(n: Int): Exp = Num(n)
  implicit def stringToId(name: String): Exp = Id(name)

  def runProg(expr: Exp): (Value, Store) = interp(expr, Map(), Map())

  test("sum of 0..4") {
    val (result, _) = runProg(
      Let("n", NewRef(0),
        Let("i", NewRef(4),
          Seq(
            Until0(
              ReadRef("i"),
              Seq(
                WriteRef("n", Plus(ReadRef("n"), ReadRef("i"))),
                WriteRef("i", Plus(ReadRef("i"), -1)))),
            ReadRef("n")))))
    assertResult(Value.Int(10))(result)
  }

  test("side effect in test") {
    val (result, _) = runProg(
      Let("d", NewRef(-2),
        Let("n", NewRef(0),
          Let("i", NewRef(4),
            Seq(
              Until0(
                Seq(
                  WriteRef("d", -1),
                  ReadRef("i")),
                Seq(
                  WriteRef("n", Plus(ReadRef("n"), ReadRef("i"))),
                  WriteRef("i", Plus(ReadRef("i"), ReadRef("d"))))),
              ReadRef("n"))))))
    assertResult(Value.Int(10))(result)
  }
