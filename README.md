## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

### Notes

Consider the following setup (under modularity):

```scala
trait Stream:
  me =>

  type Self
  type Element
  extension (self: Self)
    def next: Option[Element]
    def sameElements(using S: Stream { type Element = me.Element })(
        other: S.Self
    ): Boolean = ??? // Implementation does not matter

class LongStream(var base: Long)
given LongStream is Stream:
  type Element = Byte
  // Implementation does not matter

class ListStream[T](var base: List[T])
given [T] => ListStream[T] is Stream:
  type Element = T
  // Implementation does not matter

@main def M =
  val a = LongStream(0x030201L)
  val b = LongStream(0x030201L)
  println(a.sameElements(b))
```

The call to `sameElemens` at the end of this program has ambiguous implicit resolution. The issue is that the compiler will look for a `Stream` witness before it considers the type of its first non-contextual argument. As a result, two instances are available. Note that summoning the right witness explicitly solves the ambiguity:

```scala
println(a.sameElements(using summon[LongStream is Stream])(b)) // OK
```

I have to put the witness in the first parameter list because I want to use `S.Self` in the second. So swapping the order of the parameters is not an option. Note that taking `S` as a context bound in the more usual way does not solve the issue since it will desugar to roughly the same code:

```scala
// This is not better.
def sameElements[S: Stream { type Element = me.Element }](
    other: S.Self
): Boolean = true
```