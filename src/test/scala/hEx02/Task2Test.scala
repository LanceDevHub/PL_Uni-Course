package hEx02

import org.scalatest.funsuite.AnyFunSuite
import util.*
import util.SExpParser._

class Task2Test extends AnyFunSuite:

// TODO: add some extra test cases for Task2 hEx02

  test("testSNum") {
    // as an s-expression 42
    assertResult(
      Ext.Num(42)
    )(
      parse(SExp.Num(42))
    )
  }

  // as an s-expression (+ 1 2)
  test("testSimpleAdd") {
    assertResult(
      Ext.Plus(Ext.Num(1), Ext.Num(2))
    )(
      parse(SExp.List(List(SExp.Sym("+"), SExp.Num(1), SExp.Num(2))))
    )
  }

  // only to showcase how to use parseSExpr
  test("testParseSExpr") {
    assertResult(
      SExp.List(List(SExp.Sym("and"), SExp.List(List(SExp.Sym("or"), SExp.Sym("true"), SExp.Sym("false"))), SExp.Sym("true")))
    )(
      parseSExp("(and (or true false) true)")
    )
  }

  test("testSimpleMult") {
    assertResult(
      Ext.Mult(Ext.Num(1), Ext.Num(2))
    )(
      parse(parseSExp("(* 1 2)"))
    )
  }
