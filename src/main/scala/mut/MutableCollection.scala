package mut

import language.experimental.modularity

import scala.compiletime.deferred

/** A collection of elements that supports in-place updates. */
trait MutableCollection extends Collection:

  // /** The conforming type. */
  // type Self
  // given C: (Self is Collection) = deferred

  extension (self: Self)

    /** Assigns `e` at position `p`.
     *
     * - Complexity: O(1)
     */
    @Inout def update(p: Position, e: Element): Unit

    /** Exchanges the element at position `p` with the element at position `q`. */
    @Inout def swapAt(p: Position, q: Position): Unit

    /** Partitions `self` such that elements satisfying `isInFirstHalf` are ordered before others
     *  and returns the first position of the second half.
     *
     *  - Complexity: O(n) where n is the length of `self`.
     */
    @Inout def partition(isInFirstHalf: Element => Boolean): Position =
      self.firstPositionWhere(!isInFirstHalf(_)) match
        case None => self.end
        case Some(i) =>
          var p = i
          var q = self.positionAfter(p)
          while q != self.end do
            if isInFirstHalf(self(q)) then
              self.swapAt(p, q)
              p = self.positionAfter(p)
            q = self.positionAfter(q)
          p

  end extension

end MutableCollection
