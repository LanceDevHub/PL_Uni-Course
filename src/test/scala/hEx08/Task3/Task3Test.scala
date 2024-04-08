package hEx08.Task3

import Exp.*
import hEx08.Task3
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.Map
import scala.language.implicitConversions

class Task3Test extends AnyFunSuite:
  implicit def idToExp(id: String): Exp.Id = Id(id)
  implicit def numToExp(n: Int): Exp.Num = Num(n)

  val v = Value.Int(42)

  def emptyEnv: Environment = Map.empty
  def emptyStack: List[Environment] = emptyEnv :: Nil


  def allocAndWriteDummy(st: Store): Unit =
    val (loc, _) = st.malloc(emptyStack)
    st.updated(loc, Value.Int(-1))

  def allocAndWriteValue(st: Store, v: Value): Addr =
    val (addr, _) = st.malloc(emptyStack)
    st.updated(addr, v)
    addr

  test("compacts on gc") {
    val st = new MovingMarkAndSweepStore(10)
    allocAndWriteDummy(st)
    allocAndWriteDummy(st)
    val vLoc = allocAndWriteValue(st, v)
    allocAndWriteDummy(st)
    st.gc(Map("x" -> Value.Ref(vLoc)) :: Nil)
    assert(st.lookup(0) == Some(v), "GC does not compact")
  }

  test("updates box references on gc") {
    val st = new MovingMarkAndSweepStore(10)
    allocAndWriteDummy(st)
    val targetLoc = allocAndWriteValue(st, v)
    allocAndWriteDummy(st)
    val boxLoc = allocAndWriteValue(st, Value.Ref(targetLoc))
    st.gc(Map("b" -> Value.Ref(boxLoc)) :: Nil)
    println(st.memory.mkString("Array(", ", ", ")"))
    assert(st.lookup(1) === Some(Value.Ref(0)))
  }

  test("updates closure environments on gc") {
    val st = new MovingMarkAndSweepStore(10)
    allocAndWriteDummy(st)
    allocAndWriteDummy(st)
    val targetLoc = allocAndWriteValue(st, v)
    allocAndWriteDummy(st)
    val closureLoc = allocAndWriteValue(st, Value.Closure(List("_"), "_", Map("x" -> Value.Ref(targetLoc))))
    st.gc(Map("f" -> Value.Ref(closureLoc)) :: Nil)
    assert(st.lookup(1) == Some(Value.Closure(List("_"), "_", Map("x" -> Value.Ref(0)))))
  }

  test("updates stack on gc") {
    val st = new MovingMarkAndSweepStore(10)
    allocAndWriteDummy(st)
    allocAndWriteDummy(st)
    val vLoc = allocAndWriteValue(st, v)
    val stack = Map("x" -> Value.Ref(vLoc)) :: Nil
    st.gc(stack)
    assertResult(stack)(List(Map("x" -> (Value.Ref(0)))))
  }

  test("sample from description works") {
    val st = new MovingMarkAndSweepStore(6)
    allocAndWriteValue(st, Value.Int(23))
    allocAndWriteValue(st, Value.Int(42))
    allocAndWriteValue(st, Value.Ref(1))
    allocAndWriteValue(st, Value.Ref(2))
    allocAndWriteValue(st, Value.Int(-1))
    allocAndWriteValue(st, Value.Closure(List("_"), "_", Map("x" -> Value.Ref(4))))
    val stack = List(Map("b" -> Value.Ref(2), "f" -> Value.Ref(5)))
    st.gc(stack)
    assert(st.lookup(0) === Some(Value.Int(42)))
    assert(st.lookup(1) === Some(Value.Ref(0)))
    assert(st.lookup(2) === Some(Value.Int(-1)))
    assert(st.lookup(3) === Some(Value.Closure(List("_"), "_", Map("x" -> Value.Ref(2)))))
    assert(st.lookup(4) === None)
    assert(st.lookup(5) === None)
    assert(stack == Map("b" -> Value.Ref(1), "f" -> Value.Ref(3)) :: Nil)
  }

  test("nextFreeAddr points at correct position after gc") {
    val st = new MovingMarkAndSweepStore(6)
    allocAndWriteValue(st, Value.Int(23))
    allocAndWriteValue(st, Value.Int(42))

    allocAndWriteValue(st, Value.Ref(1))
    allocAndWriteValue(st, Value.Ref(2))
    allocAndWriteValue(st, Value.Int(-1))
    allocAndWriteValue(st, Value.Closure(List("_"), "_", Map("x" -> Value.Ref(4))))
    val stack = List(Map("b" -> Value.Ref(2), "f" -> Value.Ref(5)))
    st.gc(stack)
    assert(st.nextFreeAddr === 4)
  }
