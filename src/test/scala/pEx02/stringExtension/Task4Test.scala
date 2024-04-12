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

  // parse tests
  test("testBigIfExpr") {
    assertResult(
      Ext.If(
        Ext.BiImplication(Ext.True(), Ext.False()), Ext.Mult(Ext.Num(2), Ext.Num(3)), Ext.Plus(Ext.Num(1), Ext.Num(2))
      )
    )(
      parse(parseSExp("(if (<=> true false) (* 2 3) (+ 1 2))"))
    )
  }
    // parse string operations
  test("testSimpleStrParse") {
    assertResult(
      Ext.Str("test123")
    )(
      parse(parseSExp("(str test123)"))
    )
  }

  test("testStrEqualsParse") {
    assertResult(
      Ext.StrEquals(Ext.Str("hallo"), Ext.Str("hello"))
    )(
      parse(parseSExp("(strEquals (str hallo) (str hello))"))
    )
  }

  test("testCharAtParse") {
    assertResult(
      Ext.CharAt(Ext.Str("foobar"), Ext.Num(3))
    )(
      parse(parseSExp("(charAt (str foobar) 3)"))
    )
  }

  test("testHeadParse") {
    assertResult(
      Ext.Head(Ext.Str("hello"))
    )(
      parse(parseSExp("(head (str hello))"))
    )
  }

  test("testStartsWithParse") {
    assertResult(
      Ext.StartsWith(Ext.Str("hello"), Ext.Str("h"))
    )(
      parse(parseSExp("(startsWith (str hello) (str h))"))
    )
  }

  test("testEndsWithParse") {
    assertResult(
      Ext.EndsWith(Ext.Str("hello"), Ext.Str("l"))
    )(
      parse(parseSExp("(endsWith (str hello) (str l))"))
    )
  }

  test("testConcatParse") {
    assertResult(
      Ext.Concat(Ext.Str("hello"), Ext.Str("world"))
    )(
      parse(parseSExp("(concat (str hello) (str world))"))
    )
  }

  test("testLengthParse") {
    assertResult(
      Ext.Length(Ext.Str("hello"))
    )(
      parse(parseSExp("(length (str hello))"))
    )
  }

  //  interpreter tests

  test("testBigIfInterp") {
    assertResult(
      Value.Num(seven)
    )(
      runProg("(if (<=> true (and false true)) (* 3 2) (+ 3 4))")
    )
  }

  test("testAddInterp") {
    assertResult(
      Value.Num(seven)
    )(
      runProg("(+ 4 3)")
    )
  }
    // string interp
  test("testSimpleStrInterp") {
    assertResult(
      Value.Str("hello")
    )(
      runProg("(str hello)")
    )
  }

  test("testStrEqualsInterp") {
    assertResult(
      Value.Bool(true)
    )(
      runProg("(strEquals (str hello) (str hello))")
    )
  }

  test("testCharAtInterp") {
    assertResult(
      Value.Str("o")
    )(
      runProg("(charAt (str hello) 5)")
    )
  }

  test("testConcatInterp") {
    assertResult(
      Value.Str("helloWorld")
    )(
      runProg("(concat (str hello) (str World))")
    )
  }

  test("testLengthInterp") {
    assertResult(
      Value.Num(five)
    )(
      runProg("(length (str hello))")
    )
  }

  test("testAllTogetherInterp") {
    assertResult(
      Value.Num(seven)
    )(
      runProg("(if (endsWith (str hello) (str o)) (+ (length (str hello)) 2) (startsWith (str hello) (str h)))")
    )
  }

  test("testAllTogetherInterp2") {
    assertResult(
      Value.Bool(true)
    )(
      runProg("(if (endsWith (str hello) (str a)) (+ (length (str hello)) 2) (startsWith (str hello) (str h)))")
    )
  }








