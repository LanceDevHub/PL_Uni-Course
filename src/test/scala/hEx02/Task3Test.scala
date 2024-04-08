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
