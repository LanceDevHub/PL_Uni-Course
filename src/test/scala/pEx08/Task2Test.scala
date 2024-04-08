package pEx08

import Exp.*
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class Task2Test extends AnyFunSuite:

  implicit def intToNum(n: Int): Exp = Num(n)
  implicit def stringToId(name: String): Exp = Id(name)

  def runProg(expr: Exp): (Value, Store) = interp(expr, Map(), Map())

  test("single expression") {
    val (result, _) = runProg(SeqN(List(3)))
    assertResult(Value.Int(3))(result)
  }

  test("many simple expressions last is return value") {
    val (result, _) = runProg(SeqN(List(1, 2, 3)))
    assertResult(Value.Int(3))(result)
  }

  test("error on empty SeqNC") {
    assertThrows[Exception] { runProg(SeqN(Nil)) }
  }
