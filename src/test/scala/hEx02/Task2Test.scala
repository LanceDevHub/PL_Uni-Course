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

  // self-added test-cases
  test("testTrue") {
    assertResult(
      Ext.True()
    )(
      parse(parseSExp("true"))
    )
  }

  test("testFalse") {
    assertResult(
      Ext.False()
    )(
     parse(parseSExp("false"))
    )
  }

  test("testIfExpression") {
    assertResult(
      Ext.If(Ext.True(), Ext.Num(7), Ext.Num(3))
    )(
      parse(parseSExp("(if true 7 3)"))
    )
  }

  test("testNum") {
    assertResult(
      Ext.Num(420)
    )(
      parse(parseSExp("420"))
    )
  }

  test("testAdd") {
    assertResult(
      Ext.Plus(Ext.Num(7), Ext.Num(3))
    )(
      parse(parseSExp("(+ 7 3)"))
    )
  }

  test("testMult") {
    assertResult(
      Ext.Mult(Ext.Num(3), Ext.Num(7))
    )(
      parse(parseSExp("(* 3 7)"))
    )
  }

  test("testAnd") {
    assertResult(
      Ext.And(Ext.True(), Ext.False())
    )(
      parse(parseSExp("(and true false)"))
    )
  }

  test("testOr") {
    assertResult(
      Ext.Or(Ext.False(), Ext.True())
    )(
      parse(parseSExp("(or false true)"))
    )
  }

  test("testNot") {
    assertResult(
      Ext.Not(Ext.True())
    )(
      parse(parseSExp("(not true)"))
    )
  }

  test("testImplication") {
    assertResult(
      Ext.Implication(Ext.True(), Ext.False())
    )(
      parse(parseSExp("(=> true false)"))
    )
  }

  test("testBiImplication") {
    assertResult(
      Ext.BiImplication(Ext.True(), Ext.True())
    )(
      parse(parseSExp("(<=> true true)"))
    )
  }

  test("testBigIfExpression") {
    assertResult(
      Ext.If(
        Ext.BiImplication(Ext.True(), Ext.False()),
        Ext.And(Ext.Or(Ext.True(), Ext.False()), Ext.Implication(Ext.False(), Ext.False())),
        Ext.Or(Ext.True(), Ext.False())
      )
    )(
      parse(parseSExp("(if (<=> true false) (and (or true false) (=> false false)) (or true false))"))
    )
  }


