package mut

import language.experimental.modularity

/** A collection that supports backward traversal. */
trait BidirectionalCollection extends Collection:

  extension (self: Self)

    /** Returns the positon immediately before `p`. */
    def positionBefore(p: Position): Position

end BidirectionalCollection
