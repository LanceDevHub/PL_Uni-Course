package hEx04

def mapReduce[Input, K1, K2, V1, V2](input: List[Input], map: Input => List[(K1, V1)], reduce: (K1, List[V1]) => (K2, V2)): Map[K2, V2] = ???
