package pEx09

import pEx09.SExpr._
import pEx09.Exp._


def parse(se: SExpr): (ClassTable, Exp) = se match
  case SList(body) =>
    val cls = body.dropRight(1)
    val main = parseExp(body.last)
    (cls.map(parseClass).toMap, main)
  case _ => sys.error(s"$se could not be parsed")

def parseClass(se: SExpr): (String, ClassCore) = se match
  case SList(SSym("class") :: SSym(cname) :: SSym(superCname) :: body) =>
    val (privateMethodSE, restSE) = body.partition { case SList(SSym("private") :: SSym("method") :: _) => true; case _ => false }
    val (methodsSE, fieldsSE) = restSE.partition { case SList(SSym("method") :: _) => true; case _ => false }
    val (privateFieldsSE, publicFieldSE) = fieldsSE.partition { case SList(SSym("private") :: _) => true; case _ => false }
    val fields = publicFieldSE.map { case SSym(fname) => fname }
    val methods = (methodsSE ++ privateMethodSE).map(parseMethod)
    val exts = if (superCname == "Object") None else Some(superCname)
    (cname, ClassCore(exts, fields, methods.toMap))
  case _ => sys.error(s"$se could not be parsed")


def parseMethod(se: SExpr): (String, MethodCore) = se match
  case SList(SSym("private") :: SSym("method") :: SSym(mname) :: rest) => ???
  case SList(SSym("method") :: SSym(mname) :: rest) =>
    val params = rest.dropRight(1).map { case SSym(pname) => pname }
    val body = parseExp(rest.last)
    (mname, MethodCore(params, body))

def parseExp(se: SExpr): Exp = se match
  case SList(SSym("new") :: SSym(cname) :: args) =>
    New(cname, args.map(parseExp))
  case SList(SSym("!") :: recv :: SSym(fname) :: Nil) =>
    GetField(parseExp(recv), fname)
  case SList(recv :: SSym(mname) :: args) =>
    Invoke(parseExp(recv), mname, args.map(parseExp))
  case SSym(name) => Id(name)
  case _ => sys.error(s"$se could not be parsed")

def runProg(prog: String): Value =
  val (ctable, main) = parse(parseSExpr(prog))
  interp(main, Map(), ctable)

