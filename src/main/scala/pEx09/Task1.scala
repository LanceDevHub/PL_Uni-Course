package pEx09

enum Exp:
  case New(cname: String, args: List[Exp])
  case GetField(recv: Exp, fname: String)
  case Id(name: String)
  case Invoke(recv: Exp, mname: String, args: List[Exp])
import Exp.*

import scala.annotation.tailrec

case class MethodIsPrivateException(msg: String) extends Exception(msg)
// TODO: Adapt the MethodCore to include a flag for private methods
case class MethodCore(params: List[String], body: Exp)

case class ClassCore(exts: Option[String], fields: List[String], methods: Map[String, MethodCore])

type ClassTable = Map[String, ClassCore]
type Environment = Map[String, Value]

// Example:
//  val Box = ClassCore(Some("Object"), List("content", "content2", "content3"), Map())
//  val RedBox = ClassCore(Some("Box"), List("intensity"), Map())
//  val rd = New("RedBox", List(intensityArg, contentArg, contentArg2, contentArg3))
//  val o = Obj("RedBox", List(intensityV, contentV, contentV2, contentV3))
//  val i = GetField(o, "intensity")


/*
 * We store all field values in a single flat list, following this rule:
 *   -- the fields of a superclass precede the fields of a subclass --
 */
type Value = Obj
case class Obj(cl: String, fvals: List[Value])

def interp(e: Exp, env: Environment, ctable: ClassTable): Value = e match
  case Id(x) => env(x)

  case New(cname, args) =>
    val cl = lookupClass(ctable, cname)
    val all = allFields(cl, ctable)
    if (all.size != args.size)
      sys.error(s"Wrong number of arguments in $e")
    val argVals = args.map(interp(_, env, ctable))
    Obj(cname, argVals)

  case GetField(rcv, fname) =>
    val Obj(cname, fieldVals) = interp(rcv, env, ctable)
    val cl = lookupClass(ctable, cname)
    val all = allFields(cl, ctable)
    val ix = all.indexOf(fname)
    fieldVals(ix)

  case Invoke(recv, mname, args) =>
    val obj@Obj(cname, _) = interp(recv, env, ctable)
    /*
      1. find class
      2. find method in class
      3. prepare environment
      4. interpret body
    */
    val cl = lookupClass(ctable, cname)
    val method = lookupMethod(mname, cl, ctable).getOrElse(sys.error(s"Undefined method $cname.$mname"))
    val argVals = args.map(interp(_, env, ctable))
    val invokeEnv = method.params.zip(argVals).toMap + ("this" -> obj)
    interp(method.body, invokeEnv, ctable)


def allFields(cl: ClassCore, ctable: ClassTable): List[String] =
  cl.exts match
    case None | Some("Object") => cl.fields
    case Some(sup) => allFields(lookupClass(ctable, sup), ctable) ++ cl.fields

// TODO: Change lookupMethod to respect private methods
@tailrec
def lookupMethod(mname: String, cl: ClassCore, ctable: ClassTable): Option[MethodCore] =
  cl.methods.get(mname) match
    case Some(method) => Some(method)
    case None => cl.exts match
      case None | Some("Object") => None
      case Some(sup) => lookupMethod(mname, ctable(sup), ctable)

def lookupClass(ctable: ClassTable, cname: String): ClassCore =
  ctable.getOrElse(cname, sys.error(s"Class not found $cname"))

