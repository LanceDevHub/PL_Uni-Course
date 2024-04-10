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
