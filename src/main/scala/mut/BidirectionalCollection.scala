package mut

import language.experimental.modularity

import annotation.tailrec

// trait BidirectionalCollection1:
//   me =>

//   type Self: Collection as SelfAsCollection1
//   val SelfAsCollection = SelfAsCollection1

//   extension (self: Self)
//     def positionBefore(p: SelfAsCollection.Position): SelfAsCollection.Position

//   given SelfAsCollection.Slice is BidirectionalCollection1:
//     // val SelfAsCollection = me.SelfAsCollection.SliceIsCollection
//     extension (self: Self)
//       def positionBefore(p: Position): me.SelfAsCollection.Position = ???

/** A collection that supports backward traversal. */
trait BidirectionalCollection extends Collection:
  me =>

  extension (self: Self)

    /** Returns the positon immediately before `p`. */
    def positionBefore(p: Position): Position

    /** Returns the last element in `self` or `None` if `self` is empty. */
    def last: Option[Element] =
      if self.isEmpty then None else Some(self(self.positionBefore(self.end)))

    /** Returns the last element in `self` that satisfies `predicate`.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def lastWhere(predicate: Element => Boolean): Option[Element] =
      self.lastPositionWhere(predicate).map(self(_))

    /** Returns the position of the last element in `self` satisfying `predicate`, or `None` if
     *  `self` contains no such element.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    def lastPositionWhere(predicate: Element => Boolean): Option[Position] =
      @tailrec def loop(i: Position, s: Position): Option[Position] =
        if predicate(self(i)) then
          Some(i)
        else if i == s then
          None
        else
          loop(self.positionBefore(i), s)
      if self.isEmpty then None else loop(self.positionBefore(self.end), self.start)

    /** Returns a view presenting the elements of `self` in reverse order.
     *
     *  The returned view is sharing the storage of `self`. Any modification of `self` invalidates
     *  the view.
     */
    def reversed: ReversedCollection =
      ReversedCollection(self)

  end extension

  given Slice is BidirectionalCollection:

    type Position = me.Position
    type Element = me.Element

    extension (self: Self)
      final def start: Position =
        self.start
      final def end: Position =
        self.end
      final def positionAfter(p: Position): Position =
        me.positionAfter(self.base)(p)
      final def isWithin(p: Position, low: Position, high: Position): Boolean =
        me.isWithin(self.base)(p, low, high)
      final def apply(p: Position): Element =
        require(self.isWithin(p, self.start, self.end), "position is out of bounds")
        me.apply(self.base)(p)

      final def positionBefore(p: Position): Position =
        me.positionBefore(self.base)(p)

  end given

  /** A collection that presents the elements of another collection in reverse order. */
  final class ReversedCollection(val base: Self):

    /** Returns a view presenting the elements of `self` in reverse order. */
    final def reversed: Self =
      base

    /** Returns a hash of the salient parts of `this`. */
    final override def hashCode: Int =
      ~base.hashCode

    /** Returns `true` iff `this` is equal to `other`. */
    final override def equals(other: Any): Boolean =
      other match
        case o: ReversedCollection => (this.base == o.base)
        case _ => false

  end ReversedCollection

  given ReversedCollection is Collection:

    opaque type Position = me.Position
    type Element = me.Element

    extension (self: Self)
      final def start: Position =
        me.end(self.base)
      final def end: Position =
        me.start(self.base)
      final def positionAfter(p: Position): Position =
        me.positionBefore(self.base)(p)
      final def isWithin(p: Position, low: Position, high: Position): Boolean =
        me.isWithin(self.base)(p, low, high)
      final def apply(p: Position): Element =
        me.apply(self.base)(me.positionBefore(self.base)(p))

  end given

  given ReversedCollection is BidirectionalCollection:

    opaque type Position = me.Position
    type Element = me.Element

    extension (self: Self)
      final def start: Position =
        me.end(self.base)
      final def end: Position =
        me.start(self.base)
      final def positionAfter(p: Position): Position =
        me.positionBefore(self.base)(p)
      final def isWithin(p: Position, low: Position, high: Position): Boolean =
        me.isWithin(self.base)(p, low, high)
      final def apply(p: Position): Element =
        me.apply(self.base)(me.positionBefore(self.base)(p))

      final def positionBefore(p: Position): Position =
        me.positionAfter(self.base)(p)

  end given

end BidirectionalCollection
