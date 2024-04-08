package ex05.FoldLang

enum Exp:
  case Bool(b: Boolean)
  case Num(Int: Int)
  case Op(opString: String, lhs: Exp, rhs: Exp)
  case Id(name: String)
  case Fun(param: String, body: Exp)
  case App(fun: Exp, arg: Exp)
import ex05.FoldLang.Exp.*

def height(e: Exp): Int = ???
