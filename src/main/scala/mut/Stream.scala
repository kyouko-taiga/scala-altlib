package mut

import language.experimental.modularity

import annotation.tailrec
import scala.math.Ordering

/** A type that produces the contents of a sequence. */
trait Stream:
  me =>

  /** The conforming type. */
  type Self

  /** The type of the elements produced by the stream. */
  type Element

  extension (self: Self)

    /** Returns the next element or `None` if `self` reached the end of the iterated sequence. */
    @Inout def next: Option[Element]

    /** A value less than or equal to the number of elements in `self`. */
    def underestimatedCount: Int =
      0

    /** Returns `true` iff `self` contains an element satisfying `predicate`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def containsWhere(predicate: Element => Boolean): Boolean =
      @tailrec def loop(): Boolean =
        self.next match
          case Some(e) => if predicate(e) then true else loop()
          case None => false
      loop()

    /** Returns `true` iff `self` contains an element equal to `x`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def contains(x: Element): Boolean =
      self.containsWhere { (y) => x.equals(y) }

    /** Returns `true` iff all elements in `self` satisfy `predicate`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def allSatisfy(predicate: Element => Boolean): Boolean =
      @tailrec def loop(): Boolean =
        self.next match
          case Some(e) => if predicate(e) then loop() else false
          case None => true
      loop()

    /** Calls `action` on each element in `self`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def forEach(action: Element => Unit): Unit =
      @tailrec def loop(): Unit =
        self.next match
          case Some(e) => action(e); loop()
          case None => ()
      loop()

    /** Returns the result of combining the in `self` using `combine`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def reduce[T](initial: T)(combine: (T, Element) => T): T =
      @tailrec def loop(r: T): T =
        self.next match
          case Some(e) => loop(combine(r, e))
          case None => r
      loop(initial)

    /** Returns the result of adding the elements in `self` into `initial` using `combine`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def reduceInto[T](initial: T)(combine: (T, Element) => Unit): T =
      @tailrec def loop(r: T): T =
        self.next match
          case Some(e) => combine(r, e); loop(r)
          case None => r
      loop(initial)

    /** Returns a vector containing the results of applying `transform` on the contents of `self`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def map[T: Copyable](transform: Element => T): Vector[T] =
      var result = Vector[T]()
      result.reserveCapacity(self.underestimatedCount)
      self.reduceInto(result) { (a, e) => a.append(transform(e)) }

    /** Returns a vector with a copy of the elements in `self` that satisfy `predicate`, occurring
     *  in the same order as in `self`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def filter(predicate: Element => Boolean)(using Element is Copyable): Vector[Element] =
      ???

    /** Returns the minimum element in `self` given `ordering`, or `None` is `self` is empty.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def min(using ordering: Ordering[Element]): Option[Element] =
      @tailrec def loop(x: Element): Element =
        self.next match
          case Some(e) => loop(if ordering.compare(x, e) > 0 then e else x)
          case None => x
      self.next match
        case Some(e) => Some(loop(e))
        case None => None

    /** Returns the maximum element in `self` given `ordering`, or `None` is `self` is empty.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def max(using ordering: Ordering[Element]): Option[Element] =
      @tailrec def loop(x: Element): Element =
        self.next match
          case Some(e) => loop(if ordering.compare(x, e) < 0 then e else x)
          case None => x
      self.next match
        case Some(e) => Some(loop(e))
        case None => None

    /** Returns `true` if `self` contains the same elements as `other`, in the same order, calling
     *  `areEqual` to test if two elements are equal.
     *
     *  Elements are visited in order. The method returns as soon as one comparison fails or if it
     *  has visited all elements of the shortest collection.
     *
     *  - Complexity: O(n) where n is the length of the shortest stream.
     */
    @Inout def elementsEqualBy[S](@Inout other: S)(areEqual: (Element, Element) => Boolean)(using
        S is Stream { type Element = me.Element }
    ): Boolean =
      @tailrec def loop(): Boolean =
        self.next match
          case Some(a) => other.next match
            case Some(b) if areEqual(a, b) => loop()
            case _ => false
          case None => other.next match
            case None => true
            case _ => false
      loop()

    /** Returns `true` if `self` contains the same elements as `other`, in the same order.
     *
     *  Elements are visited in order. The method returns as soon as one comparison fails or if it
     *  has visited all elements of the shortest collection. Comparisons are done by calling
     *  `x.equals(y)` on each pairwise elements `(x, y)`, where `x` is taken from `self` and `y` is
     *  taken from `other`.
     *
     *  - Complexity: O(n) where n is the length of the shortest stream.
     */
    @Inout def elementsEqual[S](@Inout other: S)(using
        S is Stream { type Element = me.Element }
    ): Boolean =
      self.elementsEqualBy(other) { (x, y) => x.equals(y) }

    @Inout def elementsEqual2[S: Stream.OfElem[Element]](@Inout other: S): Boolean =
      self.elementsEqualBy(other) { (x, y) => x.equals(y) }

  end extension

end Stream

object Stream:
  type OfElem[E] = Stream { type Element = E }
