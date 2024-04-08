package hEx08.Task3

import scala.collection.mutable.{ArrayBuffer, ArraySeq, ListBuffer}

enum Exp:
  case Num(num : Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case App(fun: Exp, args: List[Exp])
  case Fun(params: List[String], body: Exp)
  case Id(name: String)

  case Constr(constrName: String, args: List[Exp])
  case Match(matchee: Exp, cases: List[CaseCore])

  case NewRef()
  case ReadRef(ref: Exp)
  case WriteRef(ref: Exp, newVal: Exp)

  case Seq(e1: Exp, e2: Exp) // run e1 followed by e2
import Exp.*



case class CaseCore(constrName: String, patVars: List[String], body: Exp)

type Addr = Int
type Environment = scala.collection.mutable.Map[String, Value]


trait Store:
  def malloc(stack: List[Environment]) : (Int, Store)
  def updated(index: Int, v: Value) : Store
  def lookup(index: Int) : Option[Value]
  def gc(stack: List[Environment]): Store

class Cell(val v: Value, var marked: Boolean):
  override def toString: String = s"Cell($v, $marked)"

class MovingMarkAndSweepStore(maxSize: Int) extends Store:

  val memory = Array.ofDim[Cell](maxSize)

  var free : Int = maxSize

  var nextFreeAddr : Int = 0

  def malloc(stack: List[Environment]) : (Int, Store) =
    if (free <= 0) sys.error("out of memory")

    while (memory(nextFreeAddr) != null)
      nextFreeAddr += 1
      if (nextFreeAddr == maxSize) nextFreeAddr = 0

    free -= 1
    (nextFreeAddr, this)

  def updated(index: Int, v: Value) : Store =
    memory.update(index, Cell(v, marked = false))
    this

  def lookup(index: Int): Option[Value] =
    if (index >= 0 && index < maxSize)
      Option(memory(index)).map(_.v)
    else
      None

  private def allAddrInVal(v: Value) : Set[Int] = v match
    case Value.Ref(a) => Set(a)
    case Value.Int(_) => Set()
    case Value.Closure(_, _, env) => allAddrInEnv(env)
    case Value.Constr(_, args) => args.flatMap(allAddrInVal).toSet

  private def allAddrInEnv(env: Environment): Set[Int] =
    env.values.flatMap(allAddrInVal).toSet

  private def mark(seed: Set[Int]) : Unit =
    for (i <- seed)
      memory(i).marked = true
    val allAddresses = seed flatMap (i => allAddrInVal(memory(i).v))
    val newAddresses = allAddresses filter (i => !memory(i).marked)
    if(newAddresses.nonEmpty)
      mark(newAddresses)

  /**
    * Change the implementation of sweep to perform the moving as described
    * in the task.
    */
  private def sweep(stack: List[Environment]): Unit =
    var changedReferences = Map[Int, Int]()
    for (i <- memory.indices) {
      val v = memory(i)
      if (v == null) {
        /* No work needed on an empty memory cell */
      }
      else if (v.marked) {
        /* Reset `marked` flag for the next gc */
        v.marked = false
      }
      else {
        free += 1
        memory(i) = null
      }
    }
    // make sure that changesReferences is filled with `oldAddr -> newAddr` pairs.
    stack.foreach(env => updateEnv(env, changedReferences))
    updateStore(changedReferences)

  private def updateStore(oldToNewAddr: Map[Addr, Addr]): Unit =
    for (index <- memory.indices) {
      if (memory(index) != null) updateValue(memory(index).v, oldToNewAddr)
    }

  private def updateValue(value: Value, oldToNewAddr: Map[Addr, Addr]): Unit =
    value match
      case ref@Value.Ref(addr) =>
        if (oldToNewAddr.contains(addr))
          ref.addr = oldToNewAddr(addr)
      case Value.Int(i) => // Nothing
      case Value.Closure(params, body, closureEnv) =>
        updateEnv(closureEnv, oldToNewAddr)
      case Value.Constr(constrName, args) =>
        args.foreach(updateValue(_, oldToNewAddr))

  private def updateEnv(env: Environment, oldToNewAddr: Map[Addr, Addr]): Unit =
    env.foreach((id, value) => updateValue(value, oldToNewAddr))


  def gc(stack: List[Environment]) : Store =
    println("\nSTARTING GC\nSTACK = " + stack + "\nSTORE = " + memory)
    mark(stack.flatMap(allAddrInEnv).toSet)
    sweep(stack)

    println("GC COMPLETE\nSTORE = " + memory +
      "\nNUMBER OF FREE SLOTS = " + free)
    this


enum Value:
  case Int(i: scala.Int)
  case Closure(params: List[String], body: Exp, env: Environment)
  case Constr(constrName: String, args: List[Value])
  case Ref(var addr: Addr)

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


def interp(e: Exp, stack: List[Environment], st0: Store): (Value, Store) = e match
  case Num(num) => (Value.Int(num), st0)
  case Plus(lhs, rhs) =>
    val (v1, st1) = interp(lhs, stack, st0)
    val (v2, st2) = interp(rhs, stack, st1)
    (Value.Int(v1.int + v2.int), st2)
  case Mult(lhs, rhs) =>
    val (v1, st1) = interp(lhs, stack, st0)
    val (v2, st2) = interp(rhs, stack, st1)
    (Value.Int(v1.int * v2.int), st2)
  case Fun(params, body) => (Value.Closure(params, body, stack.head), st0)
  case App(fun, args) =>
    val (v, st1) = interp(fun, stack, st0)
    val (params, body, funenv) = v.closure
    if (params.size != args.size)
      sys.error("the number of passed arguments is not equal to the number of parameters of the function")
    val (argVals, st2) = interpList(args, stack, st1)
    interp(body, (funenv ++ params.zip(argVals)) :: stack, st2)
  case Id(name) => (stack.head(name), st0)

  case Constr(c, args) =>
    val (argVals, st1) = interpList(args, stack, st0)
    (Value.Constr(c, argVals), st1)
  case Match(matchee, cases) =>
    val (v, st1) = interp(matchee, stack, st0)
    val (c, argVals) = v.constr
    val machingCase = cases.find(cas => cas.constrName == c && cas.patVars.size == argVals.size)
    machingCase match
      case Some(CaseCore(_, params, body)) => interp(body, (stack.head ++ params.zip(argVals)) :: stack, st1)
      case _ => sys.error(s"No matching case found for $matchee")

  case NewRef() =>
    val (addr, st1) = st0.malloc(stack)
    (Value.Ref(addr), st1.updated(addr, Value.Int(0)))
  case ReadRef(r) =>
    val (v, st1) = interp(r, stack, st0)
    val addr = v.ref
    val rVal = st1.lookup(addr).getOrElse(sys.error(s"Illegal reference $addr"))
    (rVal, st1)
  case WriteRef(r, e) =>
    val (refV, st1) = interp(r, stack, st0)
    val addr = refV.ref
    val (newVal, st2) = interp(e, stack, st1)
    (newVal, st2.updated(addr, newVal))

  case Seq(e1, e2) =>
    val (_, st1) = interp(e1, stack, st0)
    interp(e2, stack, st1)


def interpList(es: List[Exp], stack: List[Environment], st0: Store): (List[Value], Store) =
  es match
    case Nil => (Nil, st0)
    case e::rest =>
      val (v, st1) = interp(e, stack, st0)
      val (vs, st2) = interpList(rest, stack, st1)
      (v::vs, st2)
