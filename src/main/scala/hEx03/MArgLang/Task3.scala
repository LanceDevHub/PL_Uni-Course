package hEx03.MArgLang

import util.SExp

enum Ext:
  case Num(num: Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  case Let(name: String, expr: Ext, body: Ext)
  case App(name: String, args: List[Ext])
  case Id(name: String)


def parse(se: SExp): Ext = ???

enum Exp:
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case Let(name: String, expr: Exp, body: Exp)
  case App(name: String, args: List[Exp])
  case Id(name: String)
import Exp.*

def desugar(e: Ext): Exp = ???

case class FunDef(name: String, params: List[String], body: Exp)

def interp(e: Exp, fundefs: Map[String, FunDef]): Int = ???

def subst(e: Exp, x: String, repl: Exp): Exp = e match
  case Num(num) => Num(num)
  case Plus(lhs, rhs) => Plus(subst(lhs, x, repl), subst(rhs, x, repl))
  case Mult(lhs, rhs) => Mult(subst(lhs, x, repl), subst(rhs, x, repl))
  case App(name, args) => ???
  case Let(name, bound, body) =>
    if (x == name)
      Let(name, subst(bound, x, repl), body)
    else
      Let(name, subst(bound, x, repl), subst(body, x, repl))
  case Id(name) => if (x == name) repl else Id(name)

