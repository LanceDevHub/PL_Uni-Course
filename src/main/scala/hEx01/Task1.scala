package hEx01

enum Nat:
  case Zero()
  case Succ(n: Nat)
import Nat.*

def add(n: Nat, m: Nat): Nat = n match
  case Zero() => m
  case Succ(x) => add(x, Succ(m))
  
def mult(n: Nat, m: Nat): Nat = n match
  case Zero() => Zero()
  case Succ(Zero()) => m
  case Succ(x) => mult(x, add(m, m))

