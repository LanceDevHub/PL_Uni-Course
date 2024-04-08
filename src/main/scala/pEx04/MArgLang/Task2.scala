package pEx04.MArgLang

enum Ext:
  case Num(num: Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  case Let(name: String, expr: Ext, body: Ext)
  case App(fun: Ext, args: List[Ext])
  case Fun(params: List[String], body: Ext)
  case Id(name: String)

enum Exp:
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case Let(name: String, expr: Exp, body: Exp)
  case App(fun: Exp, arg: Exp)
  case Fun(param: String, body: Exp)
  case Id(name: String)
import Exp.*


def desugar(e: Ext): Exp = ???
