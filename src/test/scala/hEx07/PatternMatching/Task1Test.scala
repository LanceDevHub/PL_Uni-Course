package hEx07.PatternMatching

import org.scalatest.funsuite.AnyFunSuite
import util.SExpParser.parseSExp


class Task1Test extends AnyFunSuite:
  def runProg(prog: String, adtdef: List[ADTDef]): Value = interp(desugar(parse(parseSExp(prog))), Map(), adtdef)

  val bool = ADTDef("bool", List(("true", Nil), ("false", Nil)))

  val num = ADTDef("num", List(
    ("zero", Nil),
    ("succ", List("num"))))

  val testADT = ADTDef("test", List(
    ("test0", Nil),
    ("test1", List("num")),
    ("test2", List("bool", "num")),
    ("test3", List("test", "num", "bool"))))

  test("testSimple") {
    assertResult(
      Value.Constr("true", Nil)
    )(
      runProg("(true)", List(bool))
    )
  }

  test("testNum1") {
    assertResult(
      Value.Constr("zero", Nil)
    )(
      runProg("(zero)", List(bool, num))
    )
  }

  test("testNum2") {
    assertResult(
      Value.Constr("succ", List(
        Value.Constr("succ", List(
          Value.Constr("succ", List(
            Value.Constr("zero", Nil)))))))
    )(
      runProg("(succ (succ (succ (zero))))", List(bool, num))
    )
  }

  test("testMatch1") {
    assertResult(
      Value.Int(1)
    )(
      runProg("(match (true) (true 1) (false 2)))", List(bool, num))
    )
  }
