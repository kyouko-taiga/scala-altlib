## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

### Notes

#### Default associated types

In Swift we can write this:

```swift
protocol Collection {
  associatedtype Position
  associatedtype Element
  associatedtype Slice: Collection = DefaultSlice<Self>
    where Slice.Position == Position, Slice.Element == Element

  var start: Position { get }
  var end: Position { get }
  func position(after p: Position) -> Position
  func at(_ p: Position) -> Element
}

struct DefaultSlice<Base: Collection> {
  let base: Base
  let start: Base.Position
  let end: Base.Position
}

extension DefaultSlice: Collection {
  typealias Position = Base.Position
  typealias Element = Base.Element
  func position(after p: Base.Position) -> Base.Position { base.position(after: p) }
  func at(_ p: Base.Position) -> Base.Element { base.at(p) }
}
```

We can get very close in Scala using this setup:

```scala
trait Collection:
  me =>

  type Self
  type Position
  type Element

  // Note: the name is important.
  type Slice : Collection {
    type Position = me.Position
    type Element = me.Element
  } as SliceIsCollection

  extension (self: Self)
    def start: Position
    def end: Position
    def positionAfter(p: Position): Position
    def apply(p: Position): Element
    def between(low: Position, high: Position): Slice

trait CollectionWithSlice extends Collection:
  me =>

  class Slice(val base: Self, val start: Position, val end: Position):
    final def positionAfter(p: Position): Position = base.positionAfter(p)
    final def apply(p: Position): Element = base(p)

  override given Slice is CollectionWithSlice as SliceIsCollection:
    type Position = me.Position
    type Element = me.Element

    extension (self: Self)
      def start: Position = self.start
      def end: Position = self.end
      def positionAfter(p: Position): Position = self.positionAfter(p)
      def apply(p: Position): Element = self.apply(p)

  extension (self: Self)
    def between(low: Position, high: Position): Slice =
      new Slice(self, low, high)
```

That isn't ideal.
As we provide the default associated type in another trait, users will have to implement `CollectionWithSlice` rather than just `Collection` if they want the default associated type.
Switching names does not really solve the issue because now the longer name would have to be used in generic functions.
In other words, we either ask users to implement `CollectionWithSlice` in their given or they must use `CollectionWithCustomSlice` as a context bound on generic algorithms.

#### Stating conformance for refinements is tedious

The trick of exporting requirements in a trait/mixin causes a lot of boilerplate.
I think that perhaps refinements should not be expressed through subtyping.

#### Dependent givens

C.f. `Slice` and `ReversedCollection`

#### Something else

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