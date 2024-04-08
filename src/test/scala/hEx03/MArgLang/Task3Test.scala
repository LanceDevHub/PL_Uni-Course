package hEx03.MArgLang

import Exp.*
import org.scalatest.funsuite.AnyFunSuite
import util.SExpParser.*

class Task3Test extends AnyFunSuite:
  val funDefs = Map("f" -> FunDef("f", List("n"), App("g", List(Plus(Id("n"), Num(5))))),
    "g" -> FunDef("g", List("n"), Mult(Id("n"), Num(1))))

  test("testParseSingleArgApp") {
    assertResult(
      Ext.App("f", List(Ext.Plus(Ext.Mult(Ext.Num(1), Ext.Num(2)), Ext.Num(4))))
    )(
      parse(parseSExp("(app f (+ (* 1 2) 4))"))
    )
  }

  test("testParseNoArgApp") {
    assertResult(Ext.App("f", List()))(
      parse(parseSExp("(app f)")))
  }

  test("testSingleArgApp") {
    assertResult(
      10
    )(
      interp(App("f", List(Num(5))), funDefs)
    )
  }

  test("testNoArgsApp") {
    assertResult(
      2
    )(
      interp(App("c", List()), Map("c" -> FunDef("c", List(), Num(2))))
    )
  }
  
  test("testTwoArgsApp") {
    assertResult(
      3
    )(
      interp(App("f", List(Num(1), Num(2))), Map("f" -> FunDef("f", List("x", "y"), Plus(Id("x"), Id("y")))))
    )
  }
