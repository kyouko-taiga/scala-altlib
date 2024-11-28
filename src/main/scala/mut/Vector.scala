package mut

import language.experimental.captureChecking
import language.experimental.modularity

import annotation.tailrec
import java.util.Arrays
import scala.math.max
import scala.util.hashing.MurmurHash3

/** An ordered, random-access, resizable collection. */
final class Vector[T: Copyable]:

  /** The contents of the vector. */
  private var contents: Array[Any] = Array.empty

  /** The number of non-null elements `contents`. */
  private var _count: Int = 0

  /** Returns `true` iff `this` is empty. */
  final def isEmpty: Boolean =
    _count == 0

  /** Returns the number of elements in `this`. */
  final def count: Int =
    _count

  /** Returns the number of elements that `this` can contain before allocating new storage. */
  final def capacity: Int =
    contents.length

  /** Returns the element at position `p`. */
  def apply(p: Int): T =
    assert(p >= 0 && p < _count)
    contents.apply(p).asInstanceOf[T]

  /** Assigns `e` at position `p` in `self`. */
  def update(p: Int, e: T): Unit =
    assert(p >= 0 && p < _count)
    contents.update(p, e)

  /** Reserves enough storage to store `minimumCapacity` elements in `this`. */
  @Inout final def reserveCapacity(minimumCapacity: Int): Unit =
    if minimumCapacity > capacity then
      var newCapacity = max(1, capacity)
      while newCapacity < minimumCapacity do
        newCapacity = newCapacity << 1
      val newContents = new Array[Any](newCapacity)
      for i <- 0 until count do
        newContents(i) = contents(i).asInstanceOf[T].copy
      contents = newContents

  /** Adds `newElement` at the end of `this`. */
  @Inout final def append(newElement: T): Unit =
    reserveCapacity(count + 1)
    contents(count) = newElement
    _count += 1

  /** Adds the contents of `newElements` at the end of `this`. */
  @Inout final def appendAll[U: Collection { type Element = T }](newElements: U): Unit =
    reserveCapacity(count + newElements.count)
    newElements.forEach(this.append)

  /** Exchanges the element at position `p` with the element at position `q`. */
  @Inout def swapAt(p: Int, q: Int): Unit =
      val t = this.contents(p)
      this.contents(p) = this.contents(q)
      this.contents(q) = t

  /** Partitions `self` in the half-open range [`low`, `high`) such that elements satisfying
   *  `isInFirstHalf` are ordered before others and returns the first position of the second half.
   *
   *  - Complexity: O(high - range)
   */
  @Inout def partitionSlice(low: Int, high: Int)(isInFirstHalf: T => Boolean): Int =
    @tailrec def lowerBound(p: Int, q: Int): Int =
      if p < q then
        if !isInFirstHalf(this(p)) then
          upperBound(p, q - 1)
        else
          lowerBound(p + 1, q)
      else
        p  // we're done

    @tailrec def upperBound(p: Int, q: Int): Int =
      if p < q then
        if isInFirstHalf(this(q)) then
          this.swapAt(p, q)
          lowerBound(p + 1, q)
        else
          upperBound(p, q - 1)
      else
        p  // we're done

    lowerBound(low, high)

  /** Sorts the contents of `this` in the half-open range [`low`, `high`). */
  @Inout def sortSlice(low: Int, high: Int)(using ordering: Ordering[T]): Unit =
    if low < high then
      val x = this(high - 1)
      val p = partitionSlice(low, high)((y) => ordering.compare(y, x) <= 0)
      if p < high then this.sortSlice(low, p)
      if p > low then this.sortSlice(p, high)

  /** Sorts the contents of `this`. */
  @Inout def sort()(using ordering: Ordering[T]): Unit =
    this.sortSlice(0, _count)

  /** Calls `action` with the underlying storage of `this`.
   *
   *  The array passed to `action` may be larger than `this.count`. Only the values in the range
   *  [`0`, `this.count`] are defined; other values must not be accessed. the argument to `action`
   *  is only valid for the duration of `action`'s call. `this` must not be mutated before `action`
   *  returns.
   *
   *  - Complexity: O(1)
   */
  def withUnderlyingStorage[U](action: Array[T]^ => U): U =
    action(contents.asInstanceOf[Array[T]])

  /** Returns a copy of `this`. */
  final def copy: Vector[T] =
    val a = Vector[T]()
    a.reserveCapacity(count)
    for i <- 0 until this._count do
      a.contents(i) = contents(i).asInstanceOf[T].copy
    a._count = count
    a

  /** Returns a hash of the salient parts of `this`. */
  final override def hashCode: Int =
    var h = 0x811c9dc5
    for i <- 0 until this._count do
      h = MurmurHash3.mix(h, this.contents(i).hashCode)
    MurmurHash3.finalizeHash(h, this._count)

  /** Returns `true` iff `this` is equal to `other`. */
  final override def equals(other: Any): Boolean =
    @tailrec def isEqualFrom(that: Vector[?], i: Int): Boolean =
      if i == this._count then
        i == that._count
      else if (i < that._count) && (this(i) == that(i)) then
        isEqualFrom(that, i + 1)
      else
        false

    other match
      case o: Vector[_] => isEqualFrom(o, 0)
      case _ => false

  /** Returns a textual representation of `this`. */
  final override def toString: String =
    val s = new java.lang.StringBuilder("[")
    for i <- 0 until _count do
      if i != 0 then s.append(", ")
      s.append(this(i).toString)
    s.append("]")
    s.toString

object Vector:

  /** Creates an empty instance. */
  final def apply[T: Copyable](elements: T*): Vector[T] =
    val result = new Vector[T]()
    result.reserveCapacity(elements.size)
    for e <- elements do result.append(e)
    result

end Vector

given [T] => Vector[T] is Copyable:

  extension (self: Vector[T])
    def copy: Vector[T] = self.copy

given [T] => Vector[T] is MutableCollection:

  type Position = Int
  type Element = T

  extension (self: Vector[T])

    // For Collection

    def start: Int = 0
    def end: Int = self.count
    def positionAfter(p: Int): Int = p + 1
    def isWithin(p: Int, low: Int, high: Int): Boolean = (p >= low) && (p < high)
    def apply(p: Int): T = self(p)
    override def count: Int = self.count
    override def isEmpty: Boolean = self.isEmpty

    // For MutableCollection

    @Inout def update(p: Int, e: T): Unit = self.update(p, e)
    @Inout def swapAt(p: Int, q: Int): Unit = self.swapAt(p, q)

  end extension

end given
