package hEx06

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

/*class Task2Test extends AnyFunSuite:

  implicit def symbolToExp(symbol: String): Id = Id(symbol)
  implicit def intToExp(n: Int): Num = Num(n)
  
  val fib =
    Fun("x",
      LetRec("fib",
        Fun("n",
          If0(
            Add("n", -1),
            1,
            If0(
              Add("n", -2),
              1,
              Add(
                App("fib", (Add("n", -2))),
                App("fib", Add("n", -1))))
          )
        ),
        App("fib", "x")))

  val fac =
    Fun("x",
      LetRec("fac",
        Fun("n",
          If0("n",
            1,
            Mult("n", App("fac", Add("n", -1))))),
        App("fac", "x")))

  test("testFib1") {
    assertResult(Value.Int(1))(interp(App(fib, 1)))
  }

  test("testFib4") {
    assertResult(Value.Int(3))(interp(App(fib, 4)))
  }

  test("testFac0") {
    assertResult(Value.Int(1))(interp(App(fac, 0)))
  }

  test("testFac5") {
    assertResult(Value.Int(120))(interp(App(fac, 5)))
  }

  test("2. corner case of subst case Fun") {
    assertThrows[Exception] {
      interp(
        LetRec("fHavingYAsFreeVar", Fun("x", If0("x", 1, Mult("y", App("f", Add("x", -1))))),
          App(Fun("y", App("fHavingYAsFreeVar", "y")), 3))
      )
    }
  }

  test("2. corner case of subst case LetRec") {
    assertThrows[Exception] {
      interp(
        LetRec("fHavingGAsFreeVar", Fun("x", App("g", "x")),
          LetRec("g", Fun("x", App("fHavingGAsFreeVar", "x")),
            App("g", 3)
          )
        )
      )
    }
  }*/


