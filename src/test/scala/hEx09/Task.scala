package ex10

import ex10.*
import ex10.Exp.*
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class TaskTest extends AnyFunSuite with BeforeAndAfter:
  val dummyClass: ClassExp = ClassExp(None, Nil, Map.empty)
  val newDummy: Exp = New("Dummy")
  val dummyVal: Value = Value.Object("Dummy", Nil)

  val altDummyClass: ClassExp = ClassExp(None, Nil, Map.empty)
  val newAltDummy: Exp = New("AltDummy")
  val altDummyVal: Value = Value.Object("AltDummy", Nil)

  val testclassesBase = Map(
    "Dummy" -> dummyClass,
    "AltDummy" -> altDummyClass)
  var testclasses: ClassTable = Map.empty

  var store: Store = _

  before {
    testclasses = Map.empty
  }

  def runProg(e: Exp): Value =
    store = new Store(100)
    store(interp(e, Map.empty, store, testclassesBase ++ testclasses))

  implicit def symbolToId(s: String): Exp = Id(s)
  implicit def numToExprC(n: Int): Exp = Num(n)

  test("empty SeqN") {
    assertThrows[Exception](runProg(SeqN()))
  }

  test("method call") {
    testclasses = Map(
      "Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp(newDummy))))
    val result = runProg(Invoke(New("Foo"), "foo"))
    assertResult(result)(dummyVal)
  }

  test("method invocation with multiple methods") {
    val fooClass = ClassExp(None, Nil, Map(
      "bar" -> MethodExp("error"),
      "baz" -> MethodExp("error"),
      "foo" -> MethodExp(newDummy),
      "zort" -> MethodExp("error")))
    testclasses = Map("Foo" -> fooClass)
    val result = runProg(Invoke(New("Foo"), "foo"))
    assertResult(result)(dummyVal)
  }

  test("method invocation with argument access") {
    testclasses = Map("Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp("x", "x"))))
    val result = runProg(Invoke(New("Foo"), "foo", newDummy))
    assertResult(result)(dummyVal)
  }

  test("method invocation with multiple arguments") {
    testclasses = Map("Foo" -> ClassExp(None, Nil, Map(
      "foo" -> MethodExp("y", "x", "y", "z"))))
    val result = runProg(
      Invoke(New("Foo"), "foo", newAltDummy, newDummy, newAltDummy))
    assertResult(result)(dummyVal)
  }

  test("method invocation with this access") {
    testclasses = Map("Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp("this"))))
    val result = runProg(Invoke(New("Foo"), "foo"))
    assertResult(result)(Value.Object("Foo", Nil))
  }

  test("get field") {
    testclasses = Map("Foo" -> ClassExp(None, List("bar"), Map.empty))
    val result = runProg(GetField(New("Foo", newDummy), "bar"))
    assertResult(result)(dummyVal)
  }

  test("get with multiple fields") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("bar", "foo", "baz"), Map.empty))
    val result = runProg(
      GetField(New("Foo", newAltDummy, newDummy, newAltDummy), "foo"))
    assertResult(result)(dummyVal)
  }

  test("get field with name 'this'") {
    testclasses = Map("Foo" -> ClassExp(None, List("this"), Map.empty))
    val result = runProg(GetField(New("Foo", newDummy), "this"))
    assertResult(result)(dummyVal)
  }

  test("field does not shadow this") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("this"), Map("foo" -> MethodExp("this")))
    )
    val result = runProg(Invoke(New("Foo", newDummy), "foo"))
    result match
      case Value.Object(name, fLocs) =>
        assertResult(store(fLocs.head))(dummyVal)
      case _ => sys.error("result should be Value.Object")
  }

  test("arg does not shadow this") {
    testclasses = Map(
      "Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp("this", "this"))))
    val result = runProg(Invoke(New("Foo"), "foo", newDummy))
    assertResult(result)(Value.Object("Foo", Nil))
  }

  test("arg shadows field") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("bar"), Map("foo" -> MethodExp("bar", "bar"))))
    val result = runProg(Invoke(New("Foo", newAltDummy), "foo", newDummy))
    assertResult(result)(dummyVal)
  }

  test("get field from parent") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("field"), Map.empty),
      "Bar" -> ClassExp(Some("Foo"), Nil, Map.empty))
    val result = runProg(GetField(New("Bar", newDummy), "field"))
    assertResult(result)(dummyVal)
  }

  test("chield fields before parent fields") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("pField"), Map.empty),
      "Bar" -> ClassExp(Some("Foo"), List("cField"), Map.empty))
    val result = runProg(GetField(New("Bar", newDummy, newAltDummy), "pField"))
    assertResult(result)(altDummyVal)
  }

  test("access methods from parent") {
    testclasses = Map(
      "Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp(newDummy))),
      "Bar" -> ClassExp(Some("Foo"), Nil, Map.empty))
    val result = runProg(Invoke(New("Bar"), "foo"))
    assertResult(result)(dummyVal)
  }

  test("access methods from ancestor") {
    testclasses = Map(
      "Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp(newDummy))),
      "Bar" -> ClassExp(Some("Foo"), Nil, Map.empty),
      "Baz" -> ClassExp(Some("Foo"), Nil, Map.empty))
    val result = runProg(Invoke(New("Baz"), "foo"))
    assertResult(result)(dummyVal)
  }

  test("does not override field in child") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("pField"), Map.empty),
      "Bar" -> ClassExp(Some("Foo"), List("pField", "cField"), Map.empty))
    val result = runProg(
      GetField(New("Bar", newAltDummy, newDummy, newAltDummy), "cField"))
    assertResult(result)(dummyVal)
  }

  test("child field shadows parent field") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("field"), Map.empty),
      "Bar" -> ClassExp(Some("Foo"), List("field"), Map.empty))
    val result = runProg(GetField(New("Bar", newAltDummy, newDummy), "field"))
    assertResult(result)(altDummyVal)
  }

  test("override methods in child") {
    testclasses = Map(
      "Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp(newDummy))),
      "Bar" -> ClassExp(Some("Foo"), Nil, Map("foo" -> MethodExp(newAltDummy))))
    val result = runProg(Invoke(New("Bar"), "foo"))
    assertResult(result)(altDummyVal)
  }

  test("override methods in grandchild") {
    testclasses = Map(
      "Foo" -> ClassExp(None, Nil, Map("foo" -> MethodExp(newDummy))),
      "Bar" -> ClassExp(Some("Foo"), Nil, Map.empty),
      "Baz" -> ClassExp(Some("Foo"), Nil, Map("foo" -> MethodExp(newAltDummy))))
    val result = runProg(Invoke(New("Baz"), "foo"))
    assertResult(result)(altDummyVal)
  }

  test("mutable field with SetFieldC") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("field"), Map.empty))
    val result = runProg(
      Let("foo", New("Foo", newDummy),
        SeqN(
          SetField("foo", "field", newAltDummy),
          GetField("foo", "field"))))
    assertResult(result)(altDummyVal)
  }

  test("mutable field in method InvokeC") {
    testclasses = Map(
      "Foo" -> ClassExp(
        None,
        List("field"),
        Map("set" -> MethodExp(SetField("this", "field", "x"), "x"))))
    val result = runProg(
      Let("foo", New("Foo", newDummy),
        SeqN(
          Invoke("foo", "set", newAltDummy),
          GetField("foo", "field"))))
    assertResult(result)(altDummyVal)
  }

  test("method InvokeC with direct field access") {
    testclasses = Map(
      "Foo" -> ClassExp(None, List("bar"), Map("foo" -> MethodExp("bar"))))
    val result = runProg(Invoke(New("Foo", newDummy), "foo"))
    assertResult(result)(dummyVal)
  }

  test("mutable field with direct access in method InvokeC") {
    testclasses = Map(
      "Foo" -> ClassExp(
        None,
        List("field"),
        Map("set" -> MethodExp(Set("field", "x"), "x"))))
    val result = runProg(
      Let("foo", New("Foo", newDummy),
        SeqN(
          Invoke("foo", "set", newAltDummy),
          GetField("foo", "field"))))
    assertResult(result)(altDummyVal)
  }

  test("szenario 1") {
    testclasses = Map(
      "Vehicle" -> ClassExp(None, List("speed"), Map.empty),
      "Motorized" -> ClassExp(Some("Vehicle"), List("power"), Map(
        "accel" -> MethodExp(Set("speed", Add("speed", Mult("s", "power"))), "s"))),
      "Transport" -> ClassExp(Some("Motorized"), List("seats"), Map.empty))
    val result = runProg(
      Let("bus", New("Transport", 32, 5, 0), SeqN(
        Invoke("bus", "accel", 2),
        SetField("bus", "power", 3),
        Invoke("bus", "accel", 2),
        GetField("bus", "speed"))))
    assertResult(result)(Value.Num(16))
  }

  test("szenario 2") {
    testclasses = Map(
      "Vehicle" -> ClassExp(None, List("speed"), Map(
        "speed" -> MethodExp("speed"))),
      "Motorized" -> ClassExp(Some("Vehicle"), List("power"), Map(
        "accel" -> MethodExp(Set("speed", Add("speed", Mult("s", "power"))), "s"))),
      "Transport" -> ClassExp(Some("Motorized"), List("seats"), Map.empty),
      "Driver" -> ClassExp(None, Nil, Map(
        "drive" -> MethodExp(Invoke("motorized", "accel", 5), "motorized"))))
    val result = runProg(
      Let("bike", New("Transport", 1, 30, 0), SeqN(
        Let("driver", New("Driver"),
          Invoke("driver", "drive", "bike")),
        Invoke("bike", "speed"))))
    assertResult(result)(Value.Num(150))
  }

  test("szenario 3") {
    testclasses = Map(
      "Vehicle" -> ClassExp(None, List("speed"), Map(
        "speed" -> MethodExp("speed"),
        "speed_=" -> MethodExp(Set("speed", "x"), "x"))),
      "Motorized" -> ClassExp(Some("Vehicle"), List("power"), Map(
        "accel" -> MethodExp(
          Invoke("this", "speed_=", Add("speed", Mult("s", "power"))),
          "s"))),
      "Transport" -> ClassExp(Some("Motorized"), List("seats"), Map.empty),
      "Driver" -> ClassExp(None, Nil, Map(
        "drive" -> MethodExp(Invoke("motorized", "accel", 5), "motorized"))))
    val result = runProg(
      Let("bike", New("Transport", 1, 30, 0), SeqN(
        Let("driver", New("Driver"), SeqN(
          Invoke("driver", "drive", "bike"),
          Invoke("driver", "drive", "bike"))),
        Invoke("bike", "speed"))))
    assertResult(result)(Value.Num(300))
  }

  test("szenario 4") {
    testclasses = Map(
      "Vehicle" -> ClassExp(None, List("speed"), Map(
        "speed" -> MethodExp("speed"),
        "speed_=" -> MethodExp(Set("speed", "x"), "x"))),
      "Motorized" -> ClassExp(Some("Vehicle"), List("power"), Map(
        "accel" -> MethodExp(
          Invoke("this", "speed_=", Add("speed", Mult("s", "power"))),
          "s"))),
      "Transport" -> ClassExp(Some("Motorized"), List("seats"), Map.empty),
      "Driver" -> ClassExp(None, Nil, Map(
        "drive" -> MethodExp(Invoke("motorized", "accel", 5), "motorized"))),
      "LongDriver" -> ClassExp(None, Nil, Map(
        "drive" -> MethodExp(Invoke("motorized", "accel", 10), "motorized"))))
    val result = runProg(
      Let("bike", New("Transport", 1, 30, 0), SeqN(
        Let("driver", New("Driver"), SeqN(
          Invoke("driver", "drive", "bike"),
          Set("driver", New("LongDriver")),
          Invoke("driver", "drive", "bike"))),
        Invoke("bike", "speed"))))
    assertResult(result)(Value.Num(450))
  }
