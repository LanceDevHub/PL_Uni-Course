package hEx10

import hEx10.Task2.*
import hEx10.Task2.Exp.*
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class Task2Test extends AnyFunSuite:
  
//TODO: out-comment test


  implicit def nameToId(name: String): Exp = Id(name)
  implicit def intToNum(n: Int): Exp = Num(n)

  /*
  test("test 1") {
    assertResult(
      interp(
        Let("x", 3, Fun("y", Add("x", "y"))), Map())
    )(
      Value.FClosure("y", Add("x", "y"), Map("x" -> Value.EClosure(3, Map())))
    )
  }
  
   */

  test("test 2") {
    assertResult(
      interp(Let("x", 10, Add("x", "x")), Map())
    )(
      Value.Num(20)
    )
  }

  test("test 3") {
    assertResult(
      interp(Let("inc", Fun("x", Add("x", 1)), Add(App("inc", 4), App("inc", 5))), Map())
    )(
      Value.Num(11)
    )
  }

  test("test 4") {
    assertResult(
      interp(Let("x", 3, App(Fun("y", Add("x", "y")), 4)), Map())
    )(
      Value.Num(7)
    )
  }

  test("test 5") {
    assertResult(
      interp(Let("f", App("undef", "x"), 4), Map())
    )(
      Value.Num(4)
    )
  }

  test("test 6") {
    assertResult(
      interp(App(Fun("x", 3), "y"), Map())
    )(
      Value.Num(3)
    )
  }

  test("test 7") {
    assertResult(
      interp(If0(Sub(4, 4), App(Fun("x", 3), "y"), 8), Map())
    )(
      Value.Num(3)
    )
  }
  
  /*
  test("test 8") {
    assert(
      interp(
        Let("x", Add(4, 5),
          Let("y", Add("x", "x"),
            Let("z", "y",
              Let("x", 4, "z")))
        ), Map()
      ).isInstanceOf[Value.EClosure]
    )
  }
  
   */

