package pEx02.stringExtension

import pEx02.stringExtension.Exp.*
import org.scalatest.funsuite.AnyFunSuite
import util.*
import util.SExpParser.parseSExp

class Task4Test extends AnyFunSuite:
  def runProg(str: String): Value = interp(desugar(parse(parseSExp(str))))

  // TODO test your code
  