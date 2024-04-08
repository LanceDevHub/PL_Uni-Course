package ex05.FoldLang

import ex05.FoldLang.*
import ex05.FoldLang.Exp.*
import org.scalatest.funsuite.AnyFunSuite

class Task1Test extends AnyFunSuite:

  val bool = Bool(true)
  val num = Num(1)
  val nestedOp = Op("and", Op("not", bool, bool), bool)
  
  test("testSimple") {
    assertResult(1)(height(bool))
  }

  test("testOp") {
    assertResult(2)(height(Op("+", num, num)))
  }
  
  test("testFun") {
    assertResult(5)(height(Fun("a", Op("or", Id("a"), nestedOp))))
  }
