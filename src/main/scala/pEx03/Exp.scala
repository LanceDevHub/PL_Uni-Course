package pEx03

enum Exp:
  case Num(n: Int)
  case Plus(e1: Exp, e2: Exp)
  case Let(name: String, bound: Exp, body: Exp)
  case Id(name: String)
  
