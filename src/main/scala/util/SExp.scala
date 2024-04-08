package util

enum SExp extends Product with Serializable:
  /**
    * An integer as an S-Expression.
    */
  case Num(num: Int)

  /**
    * A symbol as an S-Expression.
    */
  case Sym(symbol: String)

  case List(list: scala.List[SExp])
