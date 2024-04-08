package pEx07

import org.scalatest.funsuite.AnyFunSuite

class Task2Test extends AnyFunSuite:

  val lengthOfList: String =
    """
      |   (fun outerListParam
      |     (rec length
      |       (fun innerListParam
      |         ???
      |       )
      |       (app length outerListParam)
      |     )
      |   )
    """.stripMargin

  test("List size 0") {
    assertResult(
      AlgebraicDataTypes.Value.Int(0)
    )(
      AlgebraicDataTypes.runProg(s"(app $lengthOfList (nil))")
    )
  }

  test("List size 3") {
    assertResult(
      AlgebraicDataTypes.Value.Int(3)
    )(
      AlgebraicDataTypes.runProg(s"""(app $lengthOfList
      |  (cons (zero)
      |    (cons (succ (zero))
      |      (cons (zero) (nil))
      |    )
      |  )
      """.stripMargin + ")")
    )
  }
