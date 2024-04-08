package hEx02

import Exp.*

enum Nat:
  case Zero()
  case Succ(n: Nat)
import Nat.*

def add(n: Nat, m: Nat): Nat = n match
  case Zero() => m
  case Succ(n1) => add(n1, Succ(m))

def mult(n: Nat, m: Nat): Nat = n match
  case Zero() => Zero()
  case Succ(n1) => add(mult(m, n1), m)

// The precondition n >= 0 has to hold
def intToPeano(n: Int): Nat = ???

enum Value:
  case Num(n: Nat)
  case Bool(b: Boolean)

def interp(expr: Exp): Value = ???
