package hEx02

import org.scalatest.funsuite.AnyFunSuite
import util.SExpParser._
import Nat.*
class Task3Test extends AnyFunSuite:




  test("testBoolExpr") {
    assertResult(
      Value.Bool(false)
    )(
      interp(desugar(parse(parseSExp("(and (<=> true false) (or (=> true false) (not false)))"))))
    )
  }
  
  test("testIfExpr") {
    assertResult(
      Value.Num(Succ(Succ(Zero())))
    )(
      interp(desugar(parse(parseSExp("(if (<=> true false) 1 2)"))))
    )
  }

  // self-added test-cases

  val zero: Nat = Zero()
  val one: Nat = Succ(zero)
  val two: Nat = Succ(one)
  val three: Nat = Succ(two)
  val four: Nat = Succ(three)
  val five: Nat = Succ(four)
  val six: Nat = Succ(five)
  val seven: Nat = Succ(six)

  val valueZero: Value.Num = Value.Num(zero)
  val valueOne: Value.Num = Value.Num(one)
  val valueTwo: Value.Num = Value.Num(two)
  val valueThree: Value.Num = Value.Num(three)
  val valueFour: Value.Num = Value.Num(four)
  val valueFive: Value.Num = Value.Num(five)
  val valueSix: Value.Num = Value.Num(six)
  val valueSeven: Value.Num = Value.Num(seven)



  test("testIntToPeano1") {
    assertResult(
      seven
    )(
      intToPeano(7)
    )
  }

  test("testIntToPeano2") {
    assertThrows[Exception](
      intToPeano(-7)
    )
  }

  test("testIntToPeano3") {
    assertResult(
      zero
    )(
      intToPeano(0)
    )
  }

  test("testNot") {
    assertResult(
      Value.Bool(false)
    )(
      interp(desugar(parse(parseSExp("(not true)"))))
    )
  }

  test("testAnd") {
    assertResult(
      Value.Bool(false)
    )(
      interp(desugar(parse(parseSExp("(and false true)"))))
    )
  }

  test("testOr"){
    assertResult(
      Value.Bool(true)
    )(
      interp(desugar(parse(parseSExp("(or false true)"))))
    )
  }

  test("testBoolException") {
    assertThrows[Exception](
      interp(desugar(parse(parseSExp("(and (<=> true false) (or (=> true false) (not notKnown)))"))))
    )
  }

  test("testAdd"){
    assertResult(
      valueSeven
    )(
      interp(desugar(parse(parseSExp("(+ 3 4)"))))
    )
  }

  test("testMult") {
    assertResult(
      valueSix
    )(
      interp(desugar(parse(parseSExp("(* 2 3)"))))
    )
  }

  test("testMultException") {
    assertThrows[Exception](
      interp(desugar(parse(parseSExp("(* true 5)"))))
    )
  }

  test("testTrue") {
    assertResult(
      Value.Bool(true)
    )(
      interp(desugar(parse(parseSExp("true"))))
    )
  }

  test("testFalse") {
    assertResult(
      Value.Bool(false)
    )(
      interp(desugar(parse(parseSExp("false"))))
    )
  }

  test("testAndOr") {
    assertResult(
      Value.Bool(true)
    )(
      interp(desugar(parse(parseSExp("(and (or true false) (or (and true true) false ))"))))
    )
  }

  test("testNum") {
    assertResult(
      valueSeven
    )(
      interp(desugar(parse(parseSExp("7"))))
    )
  }

  test("testNestedAddMult") {
    assertResult(
      valueSix
    )(
      interp(desugar(parse(parseSExp("(* (+ 1 1) (* (+ 1 2) 1))"))))
    )
  }

  test("testException2"){
    assertThrows[Exception](
      interp(desugar(parse(parseSExp("(if 6 3 2)"))))
    )
  }










