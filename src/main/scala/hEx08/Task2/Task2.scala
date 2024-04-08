package hEx08.Task2

enum Exp:
  case Num(num : Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case App(fun: Exp, args: List[Exp])
  case Fun(params: List[String], body: Exp)
  case Let(name: String, expr: Exp, body: Exp)

  case Id(name: String)

  case Constr(constrName: String, args: List[Exp])
  case Match(matchee: Exp, cases: List[Case])

  case NewRef(init: Exp)
  case ReadRef(ref: Exp)
  case WriteRef(ref: Exp, newVal: Exp)
  case Seq(e1: Exp, e2: Exp)

  case NewArray(size: Exp)
  case WriteArray(arrayExp: Exp, index: Exp, valueExp: Exp)
  case ReadArray(arrayExp: Exp, index: Exp)
import Exp.*

case class Case(constrName: String, patVars: List[String], body: Exp)

type Addr = Int
type Environment = Map[String, Value]
type Store = Map[Addr, Value]

enum Value:
  case Int(i: scala.Int)
  case Closure(params: List[String], body: Exp, env: Environment)
  case Constr(constrName: String, args: List[Value])
  case Ref(addr: Addr)
  case Array(addr: Addr, size: scala.Int)

  def int: scala.Int = this match
    case Int(i) => i
    case _ => sys.error("expected Value.Int but got " + this)

  def closure: (List[String], Exp, Environment) = this match
    case Closure(params, body, env) => (params, body, env)
    case _ => sys.error("expected Value.Closure but got " + this)

  def constr: (String, List[Value]) = this match
    case Constr(constrName, args) => (constrName, args)
    case _ => sys.error("expected Value.Constr but got " + this)

  def ref: Addr = this match
    case Ref(addr) => addr
    case _ => sys.error("expected Value.Ref but got " + this)

  def array: (Addr, scala.Int) = this match
    case Array(addr, size) => (addr, size)
    case _ => sys.error("expected Value.Array but got " + this)

def interp(e: Exp, env: Environment, st0: Store): (Value, Store) = e match
  case Num(num) => (Value.Int(num), st0)
  case Plus(lhs, rhs) =>
    val (v1, st1) = interp(lhs, env, st0)
    val (v2, st2) = interp(rhs, env, st1)
    (Value.Int(v1.int + v2.int), st2)
  case Mult(lhs, rhs) =>
    val (v1, st1) = interp(lhs, env, st0)
    val (v2, st2) = interp(rhs, env, st1)
    (Value.Int(v1.int * v2.int), st2)
  case Let(name, bound, body) =>
    val (boundV, st1) = interp(bound, env, st0)
    interp(body, env + (name -> boundV), st1)
  case Fun(params, body) => (Value.Closure(params, body, env), st0)
  case App(fun, args) =>
    val (v, st1) = interp(fun, env, st0)
    val (params, body, funenv) = v.closure
    if (params.size != args.size)
      sys.error("the number of passed arguments is not equal to the number of parameters of the function")
    val (argVals, st2) = interpList(args, env, st1)
    interp(body, funenv ++ params.zip(argVals), st2)
  case Id(name) => (env(name), st0)
  case Constr(c, args) =>
    val (argVals, st1) = interpList(args, env, st0)
    (Value.Constr(c, argVals), st1)
  case Match(matchee, cases) =>
    val (v, st1) = interp(matchee, env, st0)
    val (c, argVals) = v.constr
    val machingCase = cases.find(cas => cas.constrName == c && cas.patVars.size == argVals.size)
    machingCase match
      case Some(Case(_, params, body)) => interp(body, env ++ params.zip(argVals), st0)
      case _ => sys.error("No matching case found for " + matchee)
  case NewRef(init) =>
    val (initV, st1) = interp(init, env, st0)
    val addr = st1.size
    (Value.Ref(addr), st1.updated(addr, initV))
  case ReadRef(r) =>
    val (v, st1) = interp(r, env, st0)
    val addr = v.ref
    (st1(addr), st1)
  case WriteRef(r, e) =>
    val (refV, st1) = interp(r, env, st0)
    val addr = refV.ref
    val (newVal, st2) = interp(e, env, st1)
    (newVal, st2.updated(addr, newVal))
  case Seq(e1, e2) =>
    val (_, st1) = interp(e1, env, st0)
    interp(e2, env, st1)
  case NewArray(sizeExpr) => ???
  case ReadArray(arrayExpr, indexExpr) => ???
  case WriteArray(arrayExpr, indexExpr, valueExpr) => ???

def interpList(es: List[Exp], env: Environment, st0: Store): (List[Value], Store) =
  es match
    case Nil => (Nil, st0)
    case e::rest =>
      val (v, st1) = interp(e, env, st0)
      val (vs, st2) = interpList(rest, env, st1)
      (v::vs, st2)
