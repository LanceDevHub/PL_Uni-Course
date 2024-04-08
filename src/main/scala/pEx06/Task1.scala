package pEx06

type f = (Int => Int)
type ChurchNumeral = f => Int => Int

val zero: ChurchNumeral = (s: f) => ((z: Int) => z)

// 1.
val one: ChurchNumeral = s => z => s(z)
val two: ChurchNumeral = ???
val three: ChurchNumeral = ???

// 2.
val toInt: ChurchNumeral => Int = ???

// 3.
val succ: ChurchNumeral => ChurchNumeral = ???

// 4.
val add: ChurchNumeral => ChurchNumeral => ChurchNumeral = ???
val mul: ChurchNumeral => ChurchNumeral => ChurchNumeral = ???
