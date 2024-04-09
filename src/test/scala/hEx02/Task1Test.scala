
package hEx02

import org.scalatest.funsuite.AnyFunSuite
import util.SExpParser._
import Exp.*

class Task1Test extends AnyFunSuite:

  // self-added test-cases
  test("testTrue") {
    assertResult(
      True()
    )(
      desugar(Ext.True())
    )
  }

  test("testFalse") {
    assertResult(
      False()
    )(
      desugar(Ext.False())
    )
  }

  test("testIfExpression"){
    assertResult(
      If(True(), True(), False())
    )(
      desugar(Ext.If(Ext.True(), Ext.True(), Ext.False()))
    )
  }

  test("testNum") {
    assertResult(
      Num(6)
    )(
      desugar(Ext.Num(6))
    )
  }

  test("testPlus") {
    assertResult(
      Plus(Num(6), Num(6))
    )(
      desugar(Ext.Plus(Ext.Num(6), Ext.Num(6)))
    )
  }

  test("testMult") {
    assertResult(
      Mult(Num(7), Num(7))
    )(
      desugar(Ext.Mult(Ext.Num(7), Ext.Num(7)))
    )
  }

  test("testAnd") {
    assertResult(
      And(True(), False())
    )(
      desugar(Ext.And(Ext.True(), Ext.False()))
    )
  }

  test("testOr") {
    assertResult(
      Or(False(), True())
    )(
      desugar(Ext.Or(Ext.False(), Ext.True()))
    )
  }

  test("testNot") {
    assertResult(
      Not(True())
    )(
      desugar(Ext.Not(Ext.True()))
    )
  }


  // given test-cases
  
  test("testImplication") {
    assertResult(
      Or(Or(Not(True()), False()), True())
    )(
      desugar(Ext.Or(Ext.Implication(Ext.True(), Ext.False()), Ext.True()))
    )
  }

  test("testIfExpressionBiImplication") {
    assertResult(
      If(True(), Or(And(Not(True()), Not(False())), And(True(), False())), True())
    )(
      desugar(Ext.If(Ext.True(), Ext.BiImplication(Ext.True(), Ext.False()), Ext.True()))
    )
  }



