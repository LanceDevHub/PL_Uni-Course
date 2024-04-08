package hEx06

type Bool[A] = (A, A) => A

def tru[A]: Bool[A] = (t, f) => t
def fls[A]: Bool[A] = (t, f) => f

def asBoolean(b: Bool[Boolean]): Boolean = ???

def not[A](x: Bool[A]): Bool[A] = ???

def and[A](x: Bool[A], y: Bool[A]): Bool[A] = ???

def or[A](x: Bool[A], y: Bool[A]): Bool[A] = ???

def testBool[A](cond: Bool[A], x: A, y: A): A = ???
