package hEx04.LetBindLang

enum Ext:
  case Num(num: Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  case Let(name: String, expr: Ext, body: Ext)
  case App(fun: Ext, arg: Ext)
  case Fun(param: String, body: Ext)
  case Id(name: String)

enum Exp:
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case App(fun: Exp, arg: Exp)
  case Fun(param: String, body: Exp)
  case Id(name: String)
import Exp.*

def desugar(e: Ext): Exp = ???
