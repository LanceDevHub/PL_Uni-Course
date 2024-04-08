package hEx10.Task3

enum Exp:
  case Num(n: Int)
  case Add(lhs: Exp, rhs: Exp)
  case Sub(lhs: Exp, rhs: Exp)
  case Let(name: String, bound: Exp, body: Exp)
  case Id(name: String)
  case Fun(param: String, body: Exp)
  case App(fun: Exp, arg: Exp)
  case If0(cond: Exp, thn: Exp, els: Exp)
import hEx10.Task3.Exp.*

type Env = Map[String, Value]

enum Value:
  case Num(n: Int)
  case FClosure(param: String, body: Exp, env: Env)


def interp(exp: Exp, env: Env): Value = ???

def strict(v: Value): Value = ???
