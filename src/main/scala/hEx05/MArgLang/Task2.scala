package hEx05.MArgLang

enum Exp:
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case App(fun: Exp, arg: Exp)
  case Fun(param: String, body: Exp)
  case Id(name: String)
import Exp.*

enum Value:
  case Int(i: scala.Int)
  case Closure(param: String, body: Exp, env: Environment)

  def int: scala.Int = this match
    case Int(i) => i
    case _ => sys.error("expected Value.Int but got " + this)

  def closure: (String, Exp, Environment) = this match
    case Closure(param, body, env) => (param, body, env)
    case _ => sys.error("expected Value.Closure but got " + this)

type Environment = Map[String, Value]

def interp(e: Exp, env: Environment): Value = ???
