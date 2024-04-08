package hEx07.CaseIf

import Exp.*
import util.*
import util.SExpParser.*

enum Ext:
  case Num(num : Int)
  case Bool(b: Boolean)
  case NumEq(lhs: Ext, rhs: Ext)
  case Plus(lhs: Ext, rhs: Ext)
  case Minus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  case Let(name: String, expr: Ext, body: Ext)
  case App(fun: Ext, args: List[Ext])
  case Fun(params: List[String], body: Ext)
  case Id(name: String)
  case Rec(name: String, expr: Ext, body: Ext)

  case Constr(constrName: String, args: List[Ext])
  case Match(matchee: Ext, cases: List[CaseExt])

enum CaseExt:
  case WithoutCond(constrName: String, patVars: List[String], body: Ext)
  case WithCond(constrName: String, cond: Ext, patVars: List[String], body: Ext)

def parse(se: SExp): Ext = se match
  case SExp.Num(n) => Ext.Num(n)
  case SExp.Sym("true") => Ext.Bool(true)
  case SExp.Sym("false") => Ext.Bool(false)
  case SExp.List(List(SExp.Sym("="), lhs, rhs)) => Ext.NumEq(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("+"), lhs, rhs)) => Ext.Plus(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("-"), lhs, rhs)) => Ext.Minus(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("*"), lhs, rhs)) => Ext.Mult(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("let"), SExp.Sym(name), expr, body)) =>
    Ext.Let(name, parse(expr), parse(body))
  case SExp.List(SExp.Sym("app")::fun::args) =>
    Ext.App(parse(fun), args.map(parse))
  case SExp.List(SExp.Sym("fun")::paramsBody) =>
    val params = paramsBody.dropRight(1).map{ case SExp.Sym(param) => param}
    val body = paramsBody.last
    Ext.Fun(params, parse(body))
  case SExp.List(SExp.Sym("rec")::SExp.Sym(name)::bound::body::Nil) =>
    Ext.Rec(name, parse(bound), parse(body))
  case SExp.Sym(name) => Ext.Id(name)
  case SExp.List(SExp.Sym("match")::matchee::cases) => Ext.Match(parse(matchee), cases.map(parseCase))
  case SExp.List(SExp.Sym(constrName)::args) => Ext.Constr(constrName, args.map(parse))
  case _ => sys.error(s"$se could not be parsed")

def parseCase(se: SExp): CaseExt = ???

def desugar(e: Ext): Exp = e match
  case Ext.Num(num) => Num(num)
  case Ext.Bool(b) => Bool(b)
  case Ext.NumEq(lhs, rhs) => NumEq(desugar(lhs), desugar(rhs))
  case Ext.Plus(lhs, rhs) => Plus(desugar(lhs), desugar(rhs))
  case Ext.Minus(lhs, rhs) => Minus(desugar(lhs), desugar(rhs))
  case Ext.Mult(lhs, rhs) => Mult(desugar(lhs), desugar(rhs))
  case Ext.Let(name, expr, body) =>
    App(Fun(List(name), desugar(body)), List(desugar(expr)))
  case Ext.App(fun, args) => App(desugar(fun), args.map(desugar))
  case Ext.Fun(param, body) => Fun(param, desugar(body))
  case Ext.Id(name) => Id(name)
  case Ext.Rec(name, bound, body) => Rec(name, desugar(bound), desugar(body))

  case Ext.Constr(c, args) => Constr(c, args.map(desugar))
  case Ext.Match(matchee, cases) => Match(desugar(matchee), cases.map(desugarCase))

def desugarCase(cas: CaseExt): CaseIf = ???
