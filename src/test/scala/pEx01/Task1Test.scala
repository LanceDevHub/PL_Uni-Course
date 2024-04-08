package pEx01

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class Task1Test extends AnyFunSuite:
  def stringToRoman(romanNumeral: String): Roman = charListToRomanLetters(romanNumeral.toList)
  def romanStringToScalaInt(romanNumeral: String): Int = romanToScalaInt(stringToRoman(romanNumeral))

  test("2997 to roman") {
    assertResult(
      stringToRoman("MMCMXCVII")
    )(
      scalaIntToRoman(2997)
    )
  }

  test("1 to roman") {
    assertResult(
      stringToRoman("I")
    )(
      scalaIntToRoman(1)
    )
  }

  test("2997 to int") {
    assertResult(
      2997
    )(
      romanStringToScalaInt("MMCMXCVII")
    )
  }

  test("all numbers to roman and back") {
    for(i <- 1 to 3999)
      assertResult(
        i
      )(
        romanToScalaInt(scalaIntToRoman(i))
      )
  }

  test("1 to int") {
    assertResult(
      1
    )(
      romanStringToScalaInt("I")
    )
  }

  test("unknown character") {
    assertThrows[Exception](
      romanStringToScalaInt("MCMR")
    )
  }

  test("Too many I's") {
    assertThrows[Exception](
      romanStringToScalaInt("MCIIII")
    )
  }
