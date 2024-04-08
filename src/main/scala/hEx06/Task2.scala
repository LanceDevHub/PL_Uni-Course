package hEx06

enum Exp:
  case Num(n: Int)
  case Add(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case Id(name: String)
  case Fun(param: String, body: Exp)
  case If0(test: Exp, thenBody: Exp, elseBody: Exp)
  case LetRec(boundId: String, namedExpr: Exp, body: Exp)
  case App(funExpr: Exp, argExpr: Exp)
import hEx06.Exp.*

import scala.annotation.tailrec
import scala.util.Random


def interpretAndConvertToExp(exp: Exp) = interp(exp) match
  case Value.Int(n) => Num(n)
  case Value.Fun(param, body) => Fun(param, body)

enum Value:
  case Int(n: scala.Int)
  case Fun(param: String, body: Exp)

  def int: scala.Int = this match
    case Int(n) => n
    case _ => sys.error("Expected Value.Int but got " + this)

  def fun: (String, Exp) = this match
    case Fun(param, body) => (param, body)
    case _ => sys.error("Expected Value.Fun but got " + this)

def interp(expr: Exp): Value = expr match
  case Num(n) => Value.Int(n)
  case Add(lhs, rhs) => Value.Int(interp(lhs).int + interp(rhs).int)
  case Mult(lhs, rhs) => Value.Int(interp(lhs).int * interp(rhs).int)
  case LetRec(boundId, namedExp, boundBody) => ???
  case Id(name) => sys.error("found unbound id " + name)
  case Fun(param, body) => Value.Fun(param, body)
  case If0(testExpr, thenBody, elseBody) =>
    val testV = interp(testExpr)
    testV match
      case Value.Int(n) => interp(if (n == 0) thenBody else elseBody)
      case _ => sys.error("can only test numbers, but got: " + testV)
  case App(funExpr, argExpr) =>
    val funV = interp(funExpr)
    funV match
      case Value.Fun(param, body) => interp(subst(body, param, interpretAndConvertToExp(argExpr)))
      case _ => sys.error("can only apply functions, but got: " + funV)

def subst(expr: Exp, substId: String, value: Exp): Exp = expr match
  case Num(n) => expr
  case Add(lhs, rhs) =>
    Add(subst(lhs, substId, value), subst(rhs, substId, value))
  case Mult(lhs, rhs) =>
    Mult(subst(lhs, substId, value), subst(rhs, substId, value))
  case LetRec(boundId, namedExpr, body) => ???
  case Id(name) =>
    if (substId == name) value else expr
  case If0(test, thenBody, elseBody) =>
    If0(subst(test, substId, value), subst(thenBody, substId, value), subst(elseBody, substId, value))
  case App(funExpr, argExpr) => App(subst(funExpr, substId, value), subst(argExpr, substId, value))
  case Fun(param, body) =>
    if (param == substId)
      expr
    else if (!freeVars(value).contains(param))
      Fun(param, subst(body, substId, value))
    else
      val newParamName = freshName(freeVars(value) ++ freeVars(body))
      Fun(newParamName, subst(subst(body, param, Id(newParamName)), substId, value))


def freeVars(e: Exp): Set[String] = e match
  case Num(_) => Set()
  case Add(l, r) => freeVars(l) ++ freeVars(r)
  case Mult(l, r) => freeVars(l) ++ freeVars(r)
  case Id(x) => Set(x)
  case LetRec(x, bound, body) => (freeVars(bound) - x) ++ (freeVars(body) - x)
  case App(fun, arg) => freeVars(fun) ++ freeVars(arg)
  case If0(cond, thenBody, elseBody) => freeVars(cond) ++ freeVars(thenBody) ++ freeVars(elseBody)
  case Fun(param, body) => freeVars(body) - param


@tailrec
def freshName(takenNames: Set[String]): String =
  val name = Random.alphanumeric.take(6).mkString
  if (takenNames.contains(name)) freshName(takenNames) else name
