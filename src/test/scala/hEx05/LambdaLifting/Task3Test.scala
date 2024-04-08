package hEx05.LambdaLifting

import org.scalatest.funsuite.AnyFunSuite
import scala.language.implicitConversions
import hEx05.LambdaLifting.Exp.*

class Task3Test extends AnyFunSuite:
  implicit def stringToId(string: String): Exp.Id = Id(string)
  implicit def numToExp(num: Int): Exp.Num = Num(num)

  test("Single function") {
    assertResult(
      (
        LetVar("x", 3,
          App("addToX", List(5, "x"))
        ),
        Map("addToX" -> FunDef(List("num", "x"), Plus("x", "num")))
      )
    )(
      makeLocalFuncsGlobal(
        LetVar("x", 3,
          LetFun("addToX", FunDef(List("num"), Plus("x", "num")),
            App("addToX", List(5))
          )
        )
      )
    )
  }
  
  /*
  let l = 1 in
    letfun doubleThenIncAndAddToL(x) =
      (letfun inc(y) = (y + 1) in
        x + inc(x) + l
      )
    in
    doubleThenIncAndAddToL(5)
  |
  V

  {
    doubleThenIncAndAddToL(x, l) = x + inc(x) + l,
    inc(y) = y + 1
  }

  let l = 1 in
    doubleThenIncAndAddToL(5)
  */

  test("nested function in funDef.body") {
    assertResult(
      (
        LetVar("l", 1, App("doubleThenIncAndAddToL", List(5, "l"))),
        Map(
          "doubleThenIncAndAddToL" -> FunDef(List("x", "l"), Plus("x", Plus(App("inc", List("x")), "l"))),
          "inc" -> FunDef(List("y"), Plus("y", 1))
        )
      )
    )(
      makeLocalFuncsGlobal(
        LetVar("l", 1,
          LetFun("doubleThenIncAndAddToL", FunDef(List("x"),
              LetFun("inc", FunDef(List("y"), Plus("y", 1)),
                Plus("x", Plus(App("inc", List("x")), "l"))
              )
            ),
            App("doubleThenIncAndAddToL", List(5))
          )
        )
      )
    )
  }


  /*
  let l = 1 in
    letfun doubleThenIncAndAddToL(x) =
      (letfun addToX(y) = (y + x) in
        addToX(x) + 1 + l
      )
    in
    doubleThenIncAndAddToL(5)

  |
  V

  {
    doubleThenIncAndAddToL(x, l) = addToX(x, x) + 1 + l,
    addToX(y, x) = y + x
  }
  let l = 1 in
    doubleThenIncAndAddToL(5)
  */
  test("inner function uses param of outer function") {
    assertResult(
      (
        LetVar("l", 1, App("doubleThenIncAndAddToL", List(5, "l"))),
        Map(
          "doubleThenIncAndAddToL" -> FunDef(List("x", "l"), Plus(Plus(App("addToX", List("x", "x")), 1), "l")),
          "addToX" -> FunDef(List("y", "x"), Plus("y", "x"))
        )
      )
    )(
      makeLocalFuncsGlobal(
        LetVar("l", 1,
          LetFun("doubleThenIncAndAddToL", FunDef(List("x"),
            LetFun("addToX", FunDef(List("y"), Plus("y", "x")),
              Plus(Plus(App("addToX", List("x")), 1), "l")
            )
          ),
            App("doubleThenIncAndAddToL", List(5))
          )
        )
      )
    )
  }

  /*
    let x = 1 in
      let y = 2 in
        let z = 3 in
          let f(x) = x in
            let g(y) = y in
              let h(z) = z in
                (f(x) + g(y)) + h(z)

    |
    V

    {
      f(x) = x,
      g(y) = y,
      h(z) = z
    }
    let x = 1 in
        let y = 2 in
          let z = 3 in
            (f(x) + g(y)) + h(z)
    */
  test("no change of params") {
    assertResult(
      (
        LetVar("x", 1, LetVar("y", 2, LetVar("z", 3,
          Plus(Plus(
            App("f", List("x")), App("g", List("y"))), App("h", List("z"))
          )
        ))),
        Map(
          "f" -> FunDef(List("x"), "x"),
          "g" -> FunDef(List("y"), "y"),
          "h" -> FunDef(List("z"), "z"),
        )
      )
    )(
      makeLocalFuncsGlobal(
        LetVar("x", 1, LetVar("y", 2, LetVar("z", 3,
          LetFun("f", FunDef(List("x"), "x"),
            LetFun("g", FunDef(List("y"), "y"),
              LetFun("h", FunDef(List("z"), "z"),
                Plus(Plus(
                  App("f", List("x")), App("g", List("y"))), App("h", List("z")))
                )
              )
            )
          )
        ))
      )
    )
  }
