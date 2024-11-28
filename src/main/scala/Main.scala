import language.experimental.modularity

import mut.Vector
import mut.given

def f(x: AnyVal) = x

@main def M(): Unit =
  val a = Vector(1, 2, 4, 5, 3, 1, 2)
  println(a.partitionSlice(0, a.count)((s) => s <= 3))
  println(a)

  val b = Vector(1, 2, 4, 5, 3, 1, 2)
  println(b.partition((s) => s <= 3))
  println(b)

  // a.sort()
  // println(a)
  // a.forEach(println)
