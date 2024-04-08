package ex10

enum Exp:
  case Num(n: Int)
  case Add(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case Let(id: String, bound: Exp, body: Exp)

  case New(className: String, fields: Exp*)
  case GetField(objExp: Exp, fieldName: String)
  case Invoke(objExp: Exp, methodName: String, args: Exp*)
  case Id(id: String)

  case SetField(objExp: Exp, fieldName: String, exp: Exp)
  case Set(id: String, exp: Exp)
  case SeqN(exps: Exp*)
import Exp.*

type ClassTable = Map[String, ClassExp]
case class ClassExp(
                     exts: Option[String],
                     fields: List[String],
                     methods: Map[String, MethodExp])

case class MethodExp(body: Exp, params: String*)

enum Value:
  case Object(className: String, fields: List[Addr])
  case Num(n: Int)

type Addr = Int
type Environment = Map[String, Addr]

/**
  * This class represents a store and all the actions associated with it.
  * The store's size is fixed by size.
  */
class Store(size: Int):
  private val memory = new Array[Value](size)
  private var _nextFreeLocation = 0
  private var _free = size

  /**
    * Updates the store and sets the content at the passed address loc to the value vl.
    */
  def update(loc: Addr, vl: Value): Unit =
    val oldVal = memory(loc)
    memory(loc) = vl
    if (oldVal == null) _free -= 1

  /**
    * Returns the value at the passed address k.
    * See an example how apply can be used:
    * val s = Store(100).
    * ...
    * val v = s(50) (is desugared to s.apply(50))
    */
  def apply(k: Addr): Value = memory(k)

  /**
    * Allocate a new address and set the addresses content to the passed value.
    * It returns the allocted address.
    */
  def malloc(v: Value): Addr =
    val loc = findNextFreeLocation()
    update(loc, v)
    loc

  private def findNextFreeLocation(): Addr =
    if (_free <= 0) sys.error("Store is full")
    while (memory(_nextFreeLocation) != null)
      _nextFreeLocation += 1
      if (_nextFreeLocation >= size) _nextFreeLocation = 0
    _nextFreeLocation


def interp(e: Exp, env: Environment, st: Store, classTable: ClassTable): Addr = e match
  case Num(n) => st.malloc(Value.Num(n))

  case Add(lhs, rhs) =>
    val lhsV = st(interp(lhs, env, st, classTable))
    val rhsV = st(interp(rhs, env, st, classTable))
    (lhsV, rhsV) match
      case (Value.Num(a), Value.Num(b)) => st.malloc(Value.Num(a + b))
      case _ => sys.error("Can only add numbers")

  case Mult(lhs, rhs) =>
    val lhsV = st(interp(lhs, env, st, classTable))
    val rhsV = st(interp(rhs, env, st, classTable))
    (lhsV, rhsV) match
      case (Value.Num(a), Value.Num(b)) => st.malloc(Value.Num(a * b))
      case _ => sys.error("Can only multiply numbers")

  case Let(id, bound, body) =>
    val addr = interp(bound, env, st, classTable)
    interp(body, env + (id -> addr), st, classTable)

  case SeqN() => sys.error("Sequence is empty")
  case SeqN(exps*) =>
    exps.foldLeft[Addr](-1) {
      case (_, e) => interp(e, env, st, classTable)
    }

  case Id(id) => ???
  case New(className, args @ _*) => ???
  case GetField(receiver, fieldName) => ???
  case Invoke(objExp, methodName, args @ _*) => ???

  case Set(id, e) => ???
  case SetField(objExp, fieldName, e) => ???


def checkArgNum(cls: ClassExp, argNum: Int, ctable: ClassTable): Unit = cls.exts match
  case None =>
    // no super class, all arguments must be used in `cls`
    if (argNum != cls.fields.size)
      sys.error(s"Wrong number of constructor arguments in $cls")
  case Some(superCname) =>
    // we have a super class, but we need at least our own arguments
    if (argNum < cls.fields.size)
      sys.error(s"Wrong number of constructor arguments in $cls")
    else
      checkArgNum(lookupClass(ctable, superCname), argNum - cls.fields.size, ctable)

def lookupFieldIndex(cls: ClassExp, fname: String, errName: String, ctable: ClassTable): Int =
  // try to find the field in `cls`
  val localFieldIndex = cls.fields.indexOf(fname)
  if (localFieldIndex >= 0)
    localFieldIndex
  else
  // continue in the super class if there is any
    cls.exts match
      case None =>
        println(errName); sys.error(s"Field not found $errName")
      case Some(superCname) =>
        val superCls = lookupClass(ctable, superCname)
        val superIndex = lookupFieldIndex(superCls, fname, errName, ctable)
        // offset the index from the exts to skip our own fields on lookup
        superIndex + cls.fields.size

def lookupMethod(cls: ClassExp, mname: String, errName: String, ctable: ClassTable): MethodExp =
// true to find the method in cls
  cls.methods.get(mname) match
    case Some(m) => m
    case None =>
      // continue in the super class if there is any
      cls.exts match
        case None =>
          sys.error(s"Method not found: $errName")
        case Some(superCname) =>
          val superCls = lookupClass(ctable, superCname)
          lookupMethod(superCls, mname, errName, ctable)


def lookupClass(ctable: ClassTable, cname: String): ClassExp =
  ctable.getOrElse(cname,
    sys.error(s"Class not found: $cname"))
