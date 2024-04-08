package hEx05.LambdaLifting

enum Exp:
  case Num(num: Int)
  case Plus(l: Exp, r: Exp)
  case LetVar(name: String, bound: Exp, body: Exp)
  case LetFun(name: String, funDef: FunDef, body: Exp)
  case Id(name: String)
  case App(funName: String, args: List[Exp])
import Exp.*


case class FunDef(params: List[String], body: Exp)

def freeVars(ext: Exp): Set[String] = ext match
  case Exp.Num(num) => Set()
  case Exp.Plus(l, r) => freeVars(l) ++ freeVars(r)
  case Exp.LetVar(name, bound, body) => freeVars(bound) ++ (freeVars(body) - name)
  case Exp.LetFun(name, funDef, body) =>
    sys.error("Hint: Try to avoid this case by cleverly placing your recursive calls in \"makeLocalFuncsGlobal\"")
  case Exp.Id(name) => Set(name)
  case Exp.App(funName, args) => args.foldLeft(Set())((freeVarsOfPreviousArgs, arg) => freeVarsOfPreviousArgs ++ freeVars(arg))


var funDefs: Map[String, FunDef] = Map()

def addArgumentsToApps(ext: Exp, funName: String, newParameters: List[Exp.Id]): Exp =
  val rec = addArgumentsToApps(_, funName, newParameters)
  ext match
    case Exp.Num(num) => ext
    case Exp.Plus(l, r) => Exp.Plus(rec(l), rec(r))
    case Exp.LetVar(name, bound, body) => Exp.LetVar(name, rec(bound), rec(body))
    case Exp.LetFun(name, FunDef(params, funBody), body) => Exp.LetFun(name, FunDef(params, rec(funBody)), rec(body))
    case Exp.Id(name) => ext
    case Exp.App(funName, args) => Exp.App(funName, args ++ newParameters)


def makeLocalFuncsGlobal(ext: Exp): (Exp, Map[String, FunDef]) =
  var funDefs: Map[String, FunDef] = Map()

  def makeLocalFuncsGlobal(ext: Exp): Exp = ext match {
    case Exp.Num(num) => ???
    case Exp.Plus(l, r) => ???
    case Exp.LetVar(name, bound, body) => ???
    case Exp.LetFun(name, funDef, body) => ???
    case Exp.Id(name) => ???
    case Exp.App(funName, args) => ???
  }

  (makeLocalFuncsGlobal(ext), funDefs)
