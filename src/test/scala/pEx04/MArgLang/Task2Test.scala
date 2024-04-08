package pEx04.MArgLang


import Exp.*
import org.scalatest.funsuite.AnyFunSuite
import util.*
import util.SExpParser.*

import scala.language.implicitConversions

class Task2Test extends AnyFunSuite:
  implicit def intToExt(n: Int): Ext = Ext.Num(n)
  implicit def intToExpr(n: Int): Exp = Num(n)
  implicit def symbolToExt(s: String): Ext = Ext.Id(s)
  implicit def symbolToExpr(s: String): Exp = Id(s)

  /**
    * Zero-Parameter functions
    */
  test("testZeroParam") {
    assertResult(
      Fun("_", 3)
    )(
      desugar(Ext.Fun(List(), 3))
    )
  }

  /**
    * Some sample uses
    */
  test("testSample1") {
    assertResult(
      Fun("a", Fun("b", Mult("a", "b")))
    )(
      desugar(Ext.Fun(List("a", "b"), Ext.Mult("a", "b")))
    )
  }

  test("testSample2") {
    assertResult(
      App(App("f", "a"), "b")
    )(
      desugar(Ext.App("f", List("a", "b")))
    )
  }

  test("testSample3") {
    assertResult(
      App(App(Fun("a", Fun("b", Plus("a", "b"))), 1), 2)
    )(
      desugar(Ext.App(Ext.Fun(List("a", "b"), Ext.Plus("a", "b")), List(1, 2)))
    )
  }

  test("testSample4") {
    assertResult(
      App(App(Fun("a", Fun("b", Mult(1, 2))), 1), 2)
    )(
      desugar(Ext.App(Ext.Fun(List("a", "b"), Ext.Mult(1, 2)), List(1, 2)))
    )
  }
