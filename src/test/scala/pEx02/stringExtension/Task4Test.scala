package pEx02.stringExtension

import pEx02.stringExtension.Exp.*
import org.scalatest.funsuite.AnyFunSuite
import util.*
import util.SExpParser.parseSExp
import Nat.*

class Task4Test extends AnyFunSuite:

  val zero: Nat = Zero()
  val one: Nat = Succ(zero)
  val two: Nat = Succ(one)
  val three: Nat = Succ(two)
  val four: Nat = Succ(three)
  val five: Nat = Succ(four)
  val six: Nat = Succ(five)
  val seven: Nat = Succ(six)
  def runProg(str: String): Value = interp(desugar(parse(parseSExp(str))))


  // some functions of conversion
  test("testIntToPeano") {
    assertResult(
      seven
    )(
      intToPeano(7)
    )
  }

  test("testIntToPeano2") {
    assertResult(
      zero
    )(
      intToPeano(0)
    )
  }

  test("testPeanoToInt") {
    assertResult(
      7
    )(
      peanoToInt(seven)
    )
  }

  test("testPeanoToInt2") {
    assertResult(
      0
    )(
      peanoToInt(zero)
    )
  }

  test("testValueInt") {
    assertResult(
      7
    )(
      Value.Num(seven).int
    )
  }

  test("testValueStr") {
    assertResult(
      "HelloWorld"
    )(
      Value.Str("HelloWorld").str
    )
  }


  // desugar tests
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

    test("testNot") {
      assertResult(
        Not(True())
      )(
        desugar(Ext.Not(Ext.True()))
      )
    }

        // desugar string-operations

    test("testStr") {
      assertResult(
        Str("Hello")
      )(
        desugar(Ext.Str("Hello"))
      )
    }

    test("testStrEquals") {
      assertResult(
        StrEquals(Str("Hello"), Str("Hallo"))
      )(
        desugar(Ext.StrEquals(Ext.Str("Hello"), Ext.Str("Hallo")))
      )
    }

    test("testCharAt") {
      assertResult(
        CharAt(Str("Hello"), Num(5))
      )(
        desugar(Ext.CharAt(Ext.Str("Hello"), Ext.Num(5)))
      )
    }

  test("testHead") {
    assertResult(
      CharAt(Str("hello"), Num(1))
    )(
      desugar(Ext.Head(Ext.Str("hello")))
    )
  }

  test("testStartsWith") {
    assertResult(
      StrEquals(CharAt(Str("hello"), Num(1)), Str("h"))
    )(
      desugar(Ext.StartsWith(Ext.Str("hello"), Ext.Str("h")))
    )
  }

  test("testEndsWith") {
    assertResult(
      StrEquals(CharAt(Str("hello"), Length(Str("hello"))), Str("l"))
    )(
      desugar(Ext.EndsWith(Ext.Str("hello"), Ext.Str("l")))
    )
  }

  test("testConcat") {
    assertResult(
      Concat(Str("hello"), Str(" world"))
    )(
      desugar(Ext.Concat(Ext.Str("hello"), Ext.Str(" world")))
    )
  }

  test("testLength") {
    assertResult(
      Length(Str("hello"))
    )(
      desugar(Ext.Length(Ext.Str("hello")))
    )
  }


