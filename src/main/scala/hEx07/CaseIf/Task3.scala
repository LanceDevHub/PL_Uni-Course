package hEx07.CaseIf

import Exp.*

type Environment = Map[String, Value]

enum Value:
  case Int(i: scala.Int)
  case Bool(b: Boolean)
  case Closure(params: List[String], body: Exp, env: Environment)
  case Constr(constrName: String, args: List[Value])
  case Boxed(var boxed: Value)

  def int: scala.Int = this match
    case Int(i) => i
    case Boxed(v) => v.int
    case _ => sys.error("expected Value.Int but got " + this)

  def bool: Boolean = this match
    case Bool(b) => b
    case Boxed(v) => v.bool
    case _ => sys.error("expected Value.Bool but got " + this)

  def closure: (List[String], Exp, Environment) = this match
    case Closure(params, body, env) => (params, body, env)
    case Boxed(v) => v.closure
    case _ => sys.error("expected Value.Closure but got " + this)

  def constr: (String, List[Value]) = this match
    case Constr(constrName, args) => (constrName, args)
    case Boxed(v) => v.constr
    case _ => sys.error("expected Value.Constr but got " + this)

def interp(e: Exp, env: Environment, adtdef: List[ADTDef]): Value = e match
  case Num(num) => Value.Int(num)
  case Bool(b) => Value.Bool(b)
  case Plus(lhs, rhs) => Value.Int(interp(lhs, env, adtdef).int + interp(rhs, env, adtdef).int)
  case Minus(lhs, rhs) => Value.Int(interp(lhs, env, adtdef).int - interp(rhs, env, adtdef).int)
  case Mult(lhs, rhs) => Value.Int(interp(lhs, env, adtdef).int * interp(rhs, env, adtdef).int)
  case NumEq(lhs, rhs) => Value.Bool(interp(lhs, env, adtdef).int == interp(rhs, env, adtdef).int)
  case Fun(params, body) => Value.Closure(params, body, env)
  case App(fun, args) =>
    val (params, body, funenv) = interp(fun, env, adtdef).closure
    if (params.size != args.size)
      sys.error("the number of passed arguments is not equal to the number of parameters of the function")
    val argVals = args.map(interp(_, env, adtdef))
    interp(body, funenv ++ params.zip(argVals), adtdef)
  case Id(name) => env(name)
  case Rec(name, bound, body) =>
    val (fparams, fbody, fenv) = interp(bound, env, adtdef).closure
    val box: Value.Boxed = Value.Boxed(Value.Int(42))
    val fRec = Value.Closure(fparams, fbody, fenv + (name -> box))
    box.boxed = fRec
    interp(body, env + (name -> fRec), adtdef)
  case Constr(c, args) => ???
  case Match(matchee, cases) => ???