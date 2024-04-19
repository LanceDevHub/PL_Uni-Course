package hEx03.MLetLang

import Exp.*
import org.scalatest.funsuite.AnyFunSuite
import util.*
import util.SExpParser.*

class Task2Test extends AnyFunSuite:

  test("testId"){
    assertResult(
      Ext.Id("x")
    )(
      parse(parseSExp("x"))
    )
  }
  

  test("testParseSingleLetBinding") {
    assertResult(
      Ext.Let(List(("x", Ext.Num(5))), Ext.Plus(Ext.Id("x"), Ext.Num(1)))
    )(
      parse(parseSExp("(let ((x 5)) (+ x 1))"))
    )
  }

  test("testParseTwoLetBindings") {
    assertResult(
      Ext.Let(List(("x", Ext.Num(5)), ("y", Ext.Num(1))), Ext.Plus(Ext.Id("x"), Ext.Id("y")))
    )(
      parse(parseSExp("(let ((x 5) (y 1)) (+ x y))"))
    )
  }

  test("testDesugarTwoBindings") {
    assertResult(Let("x", Num(3), Let("y", Id("x"), Plus(Id("x"), Id("y"))))
    )(
      desugar(Ext.Let(List(("x", Ext.Num(3)), ("y", Ext.Id("x"))), Ext.Plus(Ext.Id("x"), Ext.Id("y"))))
    )
  }

  test("testDesugarOneBinding") {
    assertResult(Let("x", Num(3), Plus(Id("x"), Num(4)))
    )(
      desugar(Ext.Let(List(("x", Ext.Num(3))), Ext.Plus(Ext.Id("x"), Ext.Num(4))))
    )
  }
