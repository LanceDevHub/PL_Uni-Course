package pEx01

case class RomanScalaPair(roman: RomanLetter, scalaInt: Int)

type Roman = List[RomanLetter]

enum RomanLetter:
  case M
  case D
  case C
  case L
  case X
  case V
  case I
  case CM
  case CD
  case XC
  case XL
  case IX
  case IV
import RomanLetter.*


def romanToScalaInt(romanNumeral: Roman): Int =
  def romanToScalaIntHelper(inputRoman: Roman, romanLetters: List[RomanScalaPair]): Int = romanLetters match
    case Nil =>
      if (inputRoman.nonEmpty) sys.error("Invalid input")   // not everything could be parsed
      0
    case currRomanIntPair :: tailRomanLetters =>
      inputRoman match
        case Nil => 0
        case r1::tailInput if (r1 != currRomanIntPair.roman) => romanToScalaIntHelper(inputRoman, tailRomanLetters)
        case D::_ | L::_ | V::_ =>    currRomanIntPair.scalaInt + romanToScalaIntHelper(inputRoman.tail,
          romanLetters.tail.tail)  // move to C | X | I
        case CM::_ | XC::_ | IX::_ => currRomanIntPair.scalaInt + romanToScalaIntHelper(inputRoman.tail,
          romanLetters.tail.tail.tail.tail)  // move to XC | IX | EndOfList
        case CD::_ | XL::_ | IV::_ => currRomanIntPair.scalaInt + romanToScalaIntHelper(inputRoman.tail,
          romanLetters.tail.tail) // move to XC | IX | EndOfList
        case M::M::M::_ | C::C::C::_ | X::X::X::_ | I::I::I::_ => 3*currRomanIntPair.scalaInt + romanToScalaIntHelper(inputRoman.tail.tail.tail,
          romanLetters.tail) // move to CM | XC | IX | EndOfList
        case M::M::_ | C::C::_ | X::X::_ | I::I::_ => 2*currRomanIntPair.scalaInt + romanToScalaIntHelper(inputRoman.tail.tail,
          romanLetters.tail) // move to CM | XC | IX | EndOfList
        case M::_ | C::_ | X::_ | I::_ => currRomanIntPair.scalaInt + romanToScalaIntHelper(inputRoman.tail,
          romanLetters.tail) // move to CM | XC | IX | EndOfList

  romanToScalaIntHelper(romanNumeral, romanLetters)


val romanLetters: List[RomanScalaPair] = List(
  RomanScalaPair(M, 1000), RomanScalaPair(CM, 900),
  RomanScalaPair(D, 500),  RomanScalaPair(CD, 400),
  RomanScalaPair(C, 100),  RomanScalaPair(XC, 90),
  RomanScalaPair(L, 50),   RomanScalaPair(XL, 40),
  RomanScalaPair(X, 10),   RomanScalaPair(IX, 9),
  RomanScalaPair(V, 5),    RomanScalaPair(IV, 4),
  RomanScalaPair(I, 1)
)

def charListToRomanLetters(charsOfRomanLetters: List[Char]): Roman = charsOfRomanLetters match
  case Nil => List()
  case 'M' :: tail => M :: charListToRomanLetters(tail)
  case 'C' :: 'M' :: tail => CM :: charListToRomanLetters(tail)
  case 'D' :: tail => D :: charListToRomanLetters(tail)
  case 'C' :: 'D' :: tail => CD :: charListToRomanLetters(tail)
  case 'C' :: tail => C :: charListToRomanLetters(tail)
  case 'X' :: 'C' :: tail => XC :: charListToRomanLetters(tail)
  case 'L' :: tail => L :: charListToRomanLetters(tail)
  case 'X' :: 'L' :: tail => XL :: charListToRomanLetters(tail)
  case 'X' :: tail => X :: charListToRomanLetters(tail)
  case 'I' :: 'X' :: tail => IX :: charListToRomanLetters(tail)
  case 'V' :: tail => V :: charListToRomanLetters(tail)
  case 'I' :: 'V' :: tail => IV :: charListToRomanLetters(tail)
  case 'I' :: tail => I :: charListToRomanLetters(tail)
  case c :: tail => sys.error(s"Unknown character: $c")


def scalaIntToRoman(scalaInt: Int): Roman =
  def scalaIntToRomanHelper(scalaInt: Int, romanLetters: List[RomanScalaPair]): Roman = romanLetters match
    case Nil => List()
    case currRomanScalaPair :: tailRomanLetters =>
      val countCurrRoman = scalaInt / currRomanScalaPair.scalaInt
      val listOfCurrCountRomanLetters: Roman = (1 to countCurrRoman)    // creates a range from 1 to (including) countCurrRoman
        .map(_ => currRomanScalaPair.roman).toList                      // goes through the range and converts each number to currRomanScalaPair.roman
      val remainder: Int = scalaInt % currRomanScalaPair.scalaInt
      listOfCurrCountRomanLetters ++ scalaIntToRomanHelper(remainder, tailRomanLetters)

  scalaIntToRomanHelper(scalaInt, romanLetters)
