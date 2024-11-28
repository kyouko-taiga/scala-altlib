package mut

import language.experimental.captureChecking
import language.experimental.modularity

import annotation.tailrec
import scala.util.hashing.MurmurHash3

/** A collection of elements. */
trait Collection:
  me =>

  /** The conforming type. */
  type Self

  /** The type of a position in the collection. */
  type Position

  /** The type of the elements in the collection. */
  type Element

  /** The type of a slice of the collection. */
  type Slice = Collection.DefaultSlice[Position, Element]

  /** The type of streams over the collection. */
  type Stream = Collection.DefaultStream[Element]

  extension (self: Self)

    /** The position of the first element in the collection. */
    def start: Position

    /** The "past-the-end" position of the collection. */
    def end: Position

    /** The position immediately after `p`.
     *
     *  @param p A position that is defined in `self`.
     */
    def positionAfter(p: Position): Position

    /** Returns `p` offset by `d`.
     *
     *  @param p A position that is defined in `self`.
     *  @param d The offset to apply, which is positive.
     */
    def positionOffsetBy(p: Position, d: Int): Position =
      require(d >= 0)
      var result = p
      for _ <- 0 until d do result = self.positionAfter(result)
      result

    /** Returns `true` iff `p` is in the half-open range [`low`, `high`). */
    def isWithin(p: Position, low: Position, high: Position): Boolean

    /** Returns the element at position `p`.
     *
     *  @param p The position of the element to access, which is defined in `self`.
     *
     *  - Complexity: O(1)
     */
    def apply(p: Position): Element

    /** Returns the element at 0-based index `i`.
     *
     *  This method provides 0-based integer indexing regardless of the type of the collection's
     *  positions and the value of `self.start`. The position corresponding to the given index is
     *  computed by calling `self.positionOffsetBy(self.start, i)`.
     *
     *  - Complexity: O(i)
     */
    def at(i: Int): Element =
      val p = positionOffsetBy(self.start, i)
      self(p)

    /** Returns the number of elements in `self`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def count: Int =
      @tailrec def loop(i: Position, e: Position, r: Int): Int =
        if i == e then r else loop(positionAfter(i), e, r + 1)
      loop(start, end, 0)

    /** Returns `true` iff `self` is empty. */
    def isEmpty: Boolean =
      start == end

    /** Calls `action` with a slice of `self` containing the elements in the half-open range
     *  [`low`, `high`).
     *
     *  The slice passed to `action` uses the same positions for the same elements as `self`, which
     *  are within `low` and `high`. This slice is only valid for the duration of `action`'s call.
     *  `self` must not be mutated before `action` returns.
     *
     *  - Requires: `low` precedes `high` and both positions are with `self.start` and `self.end`.
     *  - Complexity: O(1)
     */
    def withSlice[T](low: Position, high: Position)(
        // FIXME: Collection.DefaultSlice[Position, Element]^ => T
        // Adding `^` makes implicit resolution fail in `CollectionSliceTests`.
        action: Collection.DefaultSlice[Position, Element] => T
    ): T =
      action(Collection.DefaultSlice(this, self, low, high))

    /** Returns an stream producing the elements in `self`. */
    def stream: Stream =
      Collection.DefaultStream(this, self)

    /** Returns the first element in `self` or `None` if `self` is empty. */
    def first: Option[Element] =
      if isEmpty then None else Some(self(start))

    /** Returns the first element in `self` that satisfies `predicate`. */
    def firstWhere(predicate: Element => Boolean): Option[Element] =
      self.firstPositionWhere(predicate).map(self(_))

    /** Calls `action` on each element in `self`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def forEach(action: Element => Unit): Unit =
      var s = self.stream
      s.forEach(action)

    /** Returns the result of combining the contents of `self` using `combine`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def reduce[T](initial: T)(combine: (T, Element) => T): T =
      var s = self.stream
      s.reduce(initial)(combine)

    /** Returns the result of adding the contents of `self` into `initial` using `combine`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def reduceInto[T](initial: T)(combine: (T, Element) => Unit): T =
      var s = self.stream
      s.reduceInto(initial)(combine)

    /** Returns a vector containing the results of applying `transform` the elements of in `self`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def map[T: Copyable](transform: Element => T): Vector[T] =
      // var s = self.stream
      // s.map(transform)
      val result = Vector[T]()
      result.reserveCapacity(self.count)
      self.reduceInto(result) { (result, e) =>
        result.append(transform(e))
      }

    /** Returns a vector with a copy of the elements in `self` that satisfy `predicate`, occurring
     *  in the same order as in `self`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def filter(predicate: Element => Boolean)(using Element is Copyable): Vector[Element] =
      val result = Vector[Element]()
      result.reserveCapacity(self.count)
      self.reduceInto(result) { (result, e) =>
        if predicate(e) then result.append(e)
      }

    /** Returns the minimum element in `self` given `ordering`, or `None` is `self` is empty.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def min(using ordering: Ordering[Element]): Option[Element] =
      var s = self.stream
      s.min(using ordering)

    /** Returns the maximum element in `self` given `ordering`, or `None` is `self` is empty.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def max(using ordering: Ordering[Element]): Option[Element] =
      var s = self.stream
      s.max(using ordering)

    /** Returns the position of the first element in `self` satisfying `predicate`, or `None` if
     *  `self` contains no such element.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def firstPositionWhere(predicate: Element => Boolean): Option[Position] =
      @tailrec def loop(i: Position, e: Position): Option[Position] =
        if i == e then
          None
        else if predicate(self(i)) then
          Some(i)
        else
          loop(positionAfter(i), e)
      loop(start, end)

    /** Returns the position of the first element in `self` that is equal to `x`, or `None` if
     *  `self` contains no such element.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def firstPositionOf(x: Element): Option[Position] =
      firstPositionWhere { (y) => x.equals(y) }

    /** Returns `true` iff `self` contains an element satisfying `predicate`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def containsWhere(predicate: Element => Boolean): Boolean =
      firstPositionWhere(predicate).isDefined

    /** Returns `true` iff `self` contains `x`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def contains(x: Element): Boolean =
      containsWhere { (y) => x.equals(y) }

    /** Returns `true` iff all elements in `self` satisfy `predicate`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def allSatisfy(predicate: Element => Boolean): Boolean =
      var s = self.stream
      s.allSatisfy(predicate)

    /** Returns `true` if `self` contains the same elements as `other`, in the same order, calling
     *  `areEqual` to test if two elements are equal.
     *
     *  Elements are visited in order. The method returns as soon as one comparison fails or if it
     *  has visited all elements of the shortest collection.
     *
     *  - Complexity: O(n) where n is the length of the shortest collection.
     */
    @Inout def elementsEqualBy[C](@Inout other: C)(areEqual: (Element, Element) => Boolean)(using
        C is Collection { type Element = me.Element }
    ): Boolean =
      var lhs = self.stream
      var rhs = other.stream
      lhs.elementsEqualBy(rhs)(areEqual)

    /** Returns `true` if `self` contains the same elements as `other`, in the same order.
     *
     *  Elements are visited in order. The method returns as soon as one comparison fails or if it
     *  has visited all elements of the shortest collection. Comparisons are done by calling
     *  `x.equals(y)` on each pairwise elements `(x, y)`, where `x` is taken from `self` and `y` is
     *  taken from `other`.
     *
     *  - Complexity: O(n) where n is the length of the shortest stream.
     */
    @Inout def elementsEqual[C](other: C)(using
        C is Collection { type Element = me.Element }
    ): Boolean =
      self.elementsEqualBy(other) { (x, y) => x.equals(y) }

  end extension

object Collection:

  // FIXME: We probably need to take `T: Collection` and track the witness.

  /** A slice of a collection.
   *
   * @param start The position of the first element in the slice.
   * @param end The "past-the-end" position of the slice.
   */
  final class DefaultSlice[P, E](
      C: Collection { type Position = P; type Element = E }, private val base: C.Self,
      val start: P, val end: P
  ):

    /** The position immediately after `p`. */
    final def positionAfter(p: P): P =
      base.positionAfter(p)

    /** Returns `true` iff `p` is in the half-open range [`low`, `high`). */
    def isWithin(p: P, low: P, high: P): Boolean =
      base.isWithin(p, low, high)

    /** Returns the element at position `p`.
     *
     *  @param p: The position of the element to access, which is defined in `self`.
     */
    final def apply(p: P): E =
      require(base.isWithin(p, start, end), "position is out of bounds")
      base(p)

    /** Returns a hash of the salient parts of `this`. */
    final override def hashCode: Int =
      var h = 0x811c9dc5
      h = MurmurHash3.mix(h, base.hashCode)
      h = MurmurHash3.mix(h, start.hashCode)
      h = MurmurHash3.mixLast(h, end.hashCode)
      MurmurHash3.finalizeHash(h, 3)

    /** Returns `true` iff `this` is equal to `other`. */
    final override def equals(other: Any): Boolean =
      other match
        case o: DefaultSlice[_, _] =>
          (this.base == o.base) && (this.start == o.start) && (this.end == o.end)
        case _ => false

  end DefaultSlice

  // /** A pair of positions describing the bounds of a slice. */
  // final class Bounds[P] private[Collection] (val low: P, val high: P):

  //   /** Returns a hash of the salient parts of `this`. */
  //   final override def hashCode: Int =
  //     this.low.hashCode ^ this.high.hashCode

  //   /** Returns `true` iff `this` is equal to `other`. */
  //   final override def equals(other: Any): Boolean =
  //     other match
  //       case o: Bounds[_] => (this.low == o.low) && (this.high == o.high)
  //       case _ => false

  // end Bounds

  /** A stream producing the elements of an arbitrary collection. */
  final class DefaultStream[E](C: Collection { type Element = E }, private val base: C.Self):

    /** The current position of the stream in `base`. */
    private var current = base.start

    /** The "past-the-end" position of `base`. */
    private val end = base.end

    /** Returns the next element or `None` if `this` reached the end of the iterated sequence. */
    @Inout final def next: Option[E] =
      if current == end then None else
        val e = base(current)
        current = base.positionAfter(current)
        Some(e)

    /** A value less than or equal to the number of elements in `self`. */
    final def underestimatedCount: Int =
      base.count

    /** Returns a hash of the salient parts of `this`. */
    final override def hashCode: Int =
      base.hashCode ^ current.hashCode

    /** Returns `true` iff `this` is equal to `other`. */
    final override def equals(other: Any): Boolean =
      other match
        case o: DefaultStream[_] => (this.base == o.base) && (this.current == o.current)
        case _ => false

  end DefaultStream

end Collection

given [P, E] => Collection.DefaultSlice[P, E] is Collection:

  type Position = P
  type Element = E

  extension (self: Self)
    def start: P = self.start
    def end: P = self.end
    def positionAfter(p: P): P = self.positionAfter(p)
    def isWithin(p: P, low: P, high: P): Boolean = self.isWithin(p, low, high)
    def apply(p: P): E = self(p)

given [E] => Collection.DefaultStream[E] is Stream:

  type Element = E

  extension (self: Self)
    @Inout def next: Option[E] = self.next
    final override def underestimatedCount: Int = self.underestimatedCount

end given