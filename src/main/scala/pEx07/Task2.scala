package pEx07

import pEx07.SExpr.*

object AlgebraicDataTypes:
  enum Exp:
    case Num(num : Int)
    case Plus(lhs: Exp, rhs: Exp)
    case Mult(lhs: Exp, rhs: Exp)
    case App(fun: Exp, args: List[Exp])
    case Fun(params: List[String], body: Exp)
    case Id(name: String)
    case Let(name: String, expr: Exp, body: Exp)
    case Rec(name: String, bound: Exp, body: Exp)

    case Constr(constrName: String, args: List[Exp])
    case Match(matchee: Exp, cases: List[Case])
  import Exp.*

  case class Case(constrName: String, patVars: List[String], body: Exp)

  def parse(se: SExpr): Exp = se match
    case SNum(n) => Exp.Num(n)
    case SList(List(SSym("+"), lhs, rhs)) => Exp.Plus(parse(lhs), parse(rhs))
    case SList(List(SSym("*"), lhs, rhs)) => Exp.Mult(parse(lhs), parse(rhs))
    case SList(List(SSym("let"), SSym(name), expr, body)) =>
      Exp.Let(name, parse(expr), parse(body))
    case SList(SSym("app") :: fun :: args) =>
      Exp.App(parse(fun), args.map(parse))
    case SList(SSym("fun") :: paramsBody) =>
      val params = paramsBody.dropRight(1).map { case SSym(param) => param }
      val body = paramsBody.last
      Exp.Fun(params.map(String(_)), parse(body))
    case SList(SSym("rec") :: SSym(name) :: bound :: body :: Nil) =>
      Exp.Rec(String(name), parse(bound), parse(body))
    case SSym(name) => Exp.Id(String(name))

    case SList(SSym("match") :: matchee :: cases) => Exp.Match(parse(matchee), cases.map(parseCase))
    case SList(SSym(constrName) :: args) => Exp.Constr(constrName, args.map(parse))

    case _ => sys.error(s"$se could not be parsed")

  def parseCase(se: SExpr): Case = se match
    case SList(SSym(constrName) :: paramsBody) =>
      val params = paramsBody.dropRight(1).map { case SSym(param) => param }
      val body = paramsBody.last
      Case(constrName, params, parse(body))
    case _ => sys.error(s"$se could not be parsed as case")

  def runProg(prog: String): Value = interp(parse(parseSExpr(prog)), Map())

  enum Value:
    case Int(i: scala.Int)
    case Closure(params: List[String], body: Exp, env: Environment)
    case Boxed(var boxed: Value)
    case Constr(constrName: String, args: List[Value])

    def int: scala.Int = this match
      case Int(i) => i
      case Boxed(boxed) => boxed.int
      case _ => sys.error("expected Value.Int but got " + this)

    def closure: (List[String], Exp, Environment) = this match
      case Closure(params, body, env) => (params, body, env)
      case Boxed(boxed) => boxed.closure
      case _ => sys.error(s"expected Value.Closure but got $this")

    def constr: (String, List[Value]) = this match
      case Constr(constrName, args) => (constrName, args)
      case Boxed(boxed) => boxed.constr
      case _ => sys.error(s"expected Value.Constr but got $this")

  type Environment = Map[String, Value]

  def interp(e: Exp, env: Environment): Value = e match
    case Num(num) => Value.Int(num)
    case Plus(lhs, rhs) => Value.Int(interp(lhs, env).int + interp(rhs, env).int)
    case Mult(lhs, rhs) => Value.Int(interp(lhs, env).int * interp(rhs, env).int)
    case Fun(params, body) => Value.Closure(params, body, env)
    case Let(name, expr, body) =>
      interp(body, env + (name -> interp(expr, env)))
    case App(fun, args) =>
      val (params, body, funenv) = interp(fun, env).closure
      if (params.size != args.size)
        sys.error("the number of passed arguments is not equal to the number of parameters of the function")
      val argVals = args.map(interp(_, env))
      interp(body, funenv ++ params.zip(argVals))
    case Id(name) => env(name)
    case Rec(name, bound, body) =>
      val (fparams,fbody,fenv) = interp(bound, env).closure
      val box: Value.Boxed = Value.Boxed(Value.Int(0))
      val fRec = Value.Closure(fparams, fbody, fenv + (name -> box))
      box.boxed = fRec
      interp(body, env + (name -> fRec))

    case Constr(c, args) => Value.Constr(c, args.map(interp(_, env)))
    case Match(matchee, cases) =>
      val (c, argVals) = interp(matchee, env).constr
      val machingCase = cases.find(cas => cas.constrName == c && cas.patVars.size == argVals.size)
      machingCase match
        case Some(Case(_, params, body)) => interp(body, env ++ params.zip(argVals))
        case _ => sys.error(s"No matching case found for $matchee")

