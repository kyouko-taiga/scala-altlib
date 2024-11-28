package mut

import language.experimental.modularity

/** A type class for values having a strict total order.
 *
 *  Instances of `Comparable` define a strict total order. That is, given two values `a` and `b`,
 *  exactly one of the following statements hold:
 *  - `a.equalsTo(b)`
 *  - `a.lessThan(b)`
 *  - `b.lessThan(a)`
 */
trait Comparable:

  /** The conforming type. */
  type Self

  // extension (self: Self)

    /** Returns `true` iff `self` is less than `other`. */
    /** Returns `true` iff `other` is less than `self`. */

end Comparable