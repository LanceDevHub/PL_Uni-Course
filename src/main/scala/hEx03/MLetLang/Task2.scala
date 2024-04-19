package hEx03.MLetLang

import util.*

enum Ext:
  case Num(num : Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  case Let(bindings: List[(String, Ext)],
           body: Ext)
  case Id(name: String)


def parse(se: SExp): Ext = se match
  case SExp.Num(n) => Ext.Num(n)
  case SExp.List(List(SExp.Sym("+"), lhs, rhs)) => Ext.Plus(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("*"), lhs, rhs)) => Ext.Mult(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("let"), SExp.List(bindings), body)) =>
    val bindingsList = bindings.map {
      case SExp.List(List(SExp.Sym(str), expr)) => (str.toString, parse(expr))
      case _ => sys.error("couldnt parse bindings")
    }.toList
    Ext.Let(bindingsList, parse(body))

  case SExp.Sym(str) => Ext.Id(str)
  case _ => sys.error("couldnt parse given SExpr....")

enum Exp:
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case Let(name: String,
           expr: Exp,
           body: Exp)
  case Id(name: String)
import Exp.*

def desugar(e: Ext): Exp = e match
  case Ext.Num(n) => Num(n)
  case Ext.Plus(lhs, rhs) => Plus(desugar(lhs), desugar(rhs))
  case Ext.Mult(lhs, rhs) => Mult(desugar(lhs), desugar(rhs))
  case Ext.Let(bindings, body) => bindings match
    case Nil => desugar(body)
    case head :: tail => head match
      case (str, expr) => Let(str, desugar(expr), desugar(Ext.Let(tail, body)))
  case Ext.Id(str) => Id(str)

