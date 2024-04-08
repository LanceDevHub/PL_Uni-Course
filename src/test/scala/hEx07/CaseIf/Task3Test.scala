package hEx07.CaseIf

import org.scalatest.funsuite.AnyFunSuite
import util.SExpParser.parseSExp
import Exp.*

class Task3Test extends AnyFunSuite:
  def runProg(prog: String, adtdef: List[ADTDef]): Value = interp(desugar(parse(parseSExp(prog))), Map(), adtdef)

  val num: ADTDef = ADTDef("num", List(
    ("zero", List()),
    ("succ", List("num"))))

  val list: ADTDef = ADTDef("list", List(
    ("nil", List()),
    ("cons", List("num", "list"))
  ))

  val numOccurrences: String =
    """
      |   (fun listParam matcheeParam
      |     (rec equals
      |       (fun peano primitive
      |         (match peano
      |           (zero (= primitive 0))
      |           (succ peanoMinusOne (app equals peanoMinusOne (- primitive 1) ))
      |         )
      |       )
      |       (rec numOccurrences
      |         (fun listParam matcheeParam
      |           (match listParam
      |             (nil 0)
      |             (cons ? (app equals val matcheeParam) val sublist (+ 1 (app numOccurrences sublist matcheeParam) ) )
      |             (cons val sublist (app numOccurrences sublist matcheeParam) )
      |           )
      |         )
      |         (app numOccurrences listParam matcheeParam)
      |       )
      |     )
      |   )
  """.stripMargin

  test("list isEmpty") {
    assertResult(
      Value.Bool(false)
    )(
      runProg(
        """
          | (app (fun list
          |   (match list
          |     (nil true)
          |     (cons _ _ false)
          |   )
          | )
          | (cons (zero) (nil))
          | )
          |""".stripMargin,
        List(list, num))
    )
  }

  val three = "(succ (succ (succ (zero))))"

  test("numOccurrences of 3") {
    assertResult(
      Value.Int(2)
    )(
      runProg(s"(app $numOccurrences (cons $three (cons $three (cons (zero) (nil)))) 3)", List(num, list))
    )
  }

  test("numOccurrences of 0") {
    assertResult(
      Value.Int(1)
    )(
      runProg(s"(app $numOccurrences (cons $three (cons $three (cons (zero) (nil)))) 0)", List(num, list))
    )
  }

  test("numOccurrences in nil") {
    assertResult(
      Value.Int(0)
    )(
      runProg(s"(app $numOccurrences (nil) 3)", List(num, list))
    )
  }
