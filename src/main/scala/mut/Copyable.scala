package mut

import language.experimental.modularity

/** A type class for values that can be copied. */
trait Copyable:

  /** The conforming type. */
  type Self

  extension (self: Self)

    /** Returns a copy of `self`. */
    def copy: Self

end Copyable

given [T <: AnyVal] => T is Copyable:
  extension (self: T) def copy: T = self

given String is Copyable:
  extension (self: String) def copy: String = self
