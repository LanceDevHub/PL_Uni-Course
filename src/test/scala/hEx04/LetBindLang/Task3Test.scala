package hEx04.LetBindLang


import Exp.*
import org.scalatest.funsuite.AnyFunSuite
import util.*
import util.SExpParser.*

import scala.language.implicitConversions


class Task3Test extends AnyFunSuite:

  implicit def numToExt(n: Int): Ext.Num = Ext.Num(n)
  implicit def numToExpr(n: Int): Exp.Num = Num(n)
  implicit def symbolToExt(s: String): Ext = Ext.Id(s)
  implicit def symbolToExpr(s: String): Exp = Id(s)

  def parse(se: SExp): Ext = se match
    case SExp.Num(n) => Ext.Num(n)
    case SExp.List(List(SExp.Sym("+"), lhs, rhs)) => Ext.Plus(parse(lhs), parse(rhs))
    case SExp.List(List(SExp.Sym("*"), lhs, rhs)) => Ext.Mult(parse(lhs), parse(rhs))
    case SExp.List(List(SExp.Sym("let"), SExp.Sym(name), expr, body)) =>
      Ext.Let(name, parse(expr), parse(body))
    case SExp.List(List(SExp.Sym("app"), fun, arg)) =>
      Ext.App(parse(fun), parse(arg))
    case SExp.List(List(SExp.Sym("fun"), SExp.Sym(param), body)) =>
      Ext.Fun(String(param), parse(body))
    case SExp.Sym(name) => Ext.Id(String(name))
    case _ => sys.error(s"$se could not be parsed")


  test("testLetFunInBody") {
    assertResult(
      App(Fun("x", Fun("y", Plus("x", "y"))), 3)
    )(
      desugar(Ext.Let("x", 3, Ext.Fun("y", Ext.Plus("x", "y"))))
    )
  }

  test("testLet") {
    assertResult(App(Fun("x", "x"), 3)
    )(
      desugar(Ext.Let("x", 3, "x"))
    )
  }

  test("testSingleLet") {
    assertResult(
      App(Fun("x", Mult(Id("x"), Id("x"))), Plus(Num(1), Num(2)))
    )(
      desugar(parse(parseSExp("(let x (+ 1 2) (* x x))")))
    )
  }
