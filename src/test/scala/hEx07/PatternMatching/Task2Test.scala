package hEx07.PatternMatching

import org.scalatest.funsuite.AnyFunSuite
import util.SExpParser.parseSExp

class Task2Test extends AnyFunSuite:
  def runProg(prog: String, adtdef: List[ADTDef]): Value = interp(desugar(parse(parseSExp(prog))), Map(), adtdef)

  val num: ADTDef = ADTDef("num", List(
    ("zero", List()),
    ("succ", List("num"))))

  val binTree: ADTDef = ???

  val sizeOfBinTree: String =
    """
      | (fun tree
      |   ???
      | )
    """.stripMargin

  test("constructBinTree") {
    assertResult(
      Value.Constr("node", List(
        Value.Constr("zero", List()),
        Value.Constr("leaf", List()),
        Value.Constr("leaf", List())
      ))
    )(
      runProg("(node (zero) (leaf) (leaf))", List(binTree, num))
    )
  }

  test("sizeBinTree5") {
    assertResult(
      Value.Int(5)
    )(
      runProg(s"(app $sizeOfBinTree " +
        "(node (succ (zero)) (node (zero) (leaf) (leaf)) (leaf))" +
        ")", List(num, binTree))
    )
  }
