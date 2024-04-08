package hEx03.MLetLang

import util.*

enum Ext:
  case Num(num : Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  case Let(bindings: List[(String, Ext)],
           body: Ext)
  case Id(name: String)


def parse(se: SExp): Ext = ???

enum Exp:
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case Let(name: String,
           expr: Exp,
           body: Exp)
  case Id(name: String)
import Exp.*

def desugar(e: Ext): Exp = ???

