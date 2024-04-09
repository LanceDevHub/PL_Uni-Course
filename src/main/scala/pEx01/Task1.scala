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


def romanToScalaInt(romanNumeral: Roman): Int = {
  def romanToScalaIntHelper(inputRoman: Roman, romanLetters: List[RomanScalaPair]): Int = {
      romanLetters match
        case Nil =>
          if (inputRoman.nonEmpty) sys.error("wrong input.") // if we got to the end of RomanScalaPair but input not empty
          else 0  // if we got to the end of RomanScalaPair and input is empty

        // if we didnt got to the end of RomanScalaPair, we start matching inputRoman
        case currRomPair :: tailRomPair => inputRoman match
          case Nil => 0
          // if the currentRomPair.roman doesnt matches the current inputRoman.head go to the next RomPair
          case r1 :: tail if (r1 != currRomPair.roman) => romanToScalaIntHelper(inputRoman, tailRomPair)

          // if the currenRomPair.roman matches the inputRoman.head
          case M :: M :: M :: _ | C :: C :: C :: _ | X :: X :: X :: _| I :: I :: I :: _
            => 3 * currRomPair.scalaInt + romanToScalaIntHelper(inputRoman.tail.tail.tail, tailRomPair)
            // move to CM | XC | IX | EndOfList
          case M :: M :: _ | C :: C :: _ | X :: X :: _| I :: I :: _
            => 2 * currRomPair.scalaInt + romanToScalaIntHelper(inputRoman.tail.tail, tailRomPair)
            // move to CM | XC | IX | EndOfList
          case M :: _ | C :: _ | X :: _| I :: _
            => currRomPair.scalaInt + romanToScalaIntHelper(inputRoman.tail, tailRomPair)
            // move to CM | XC | IX | EndOfList
          case CM :: _ | XC :: _ | IX :: _
            => currRomPair.scalaInt + romanToScalaIntHelper(inputRoman.tail, tailRomPair.tail.tail.tail)
            // move to XC | IX | EndOfList
          case D :: _ | L :: _ | V :: _
            => currRomPair.scalaInt + romanToScalaIntHelper(inputRoman.tail, tailRomPair.tail)
            // move to C | X | I | EndOfList
          case CD :: _ | XL :: _ | IV :: _
            => currRomPair.scalaInt + romanToScalaIntHelper(inputRoman.tail, tailRomPair.tail)
            // move to XC | IX | EndOfList
          case _ => sys.error("wrong input given.")
            // exception
  }
  romanToScalaIntHelper(romanNumeral, romanLetters)
}


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
  case c => sys.error(s"unknown character: $c")

def scalaIntToRoman(scalaInt: Int): Roman = {
  def scalaIntToRomanHelper(scalaInt: Int, romanLetters: List[RomanScalaPair]): Roman = {
    romanLetters match
      case Nil => List() // if we got to the end of our RomanScalaPair -> end of recursion

      case currPair :: tailPair =>
        val currenCount = scalaInt / currPair.scalaInt
        val listOfCurrentLetters = (1 to currenCount).map(_ => currPair.roman).toList
        val remainder = scalaInt % currPair.scalaInt
        listOfCurrentLetters ++ scalaIntToRomanHelper(remainder, tailPair)
  }
  scalaIntToRomanHelper(scalaInt, romanLetters)
}