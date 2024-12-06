import language.experimental.modularity

import mut.{Vector, BidirectionalCollection}
import mut.given

class BidirectionalCollectionTests extends munit.FunSuite:

  test("last"):
    assertEquals(0L.last, None)
    assertEquals(0x010203L.last, Some(1.toByte))

  test("lastWhere"):
    val a = 0x010203L
    assertEquals(a.lastWhere(_ > 1), Some(2.toByte))
    assertEquals(a.lastWhere(_ > 5), None)

  test("ReversedCollection.equals"):
    val a = 0x010203L.reversed
    val b = 0x030201L.reversed
    assert(a == 0x010203L.reversed)
    assert(b != 0x010203L.reversed)

  test("ReversedCollection.hashCode"):
    val a = 0x010203L.reversed
    val b = 0x030201L.reversed
    assert(a.hashCode == 0x010203L.reversed.hashCode)
    assert(b.hashCode != 0x010203L.reversed.hashCode)

  test("ReversedCollection is Collection"):
    val a = 0x010203L
    assert(a.reversed.elementsEqual(0x030201L))

  test("ReversedCollection is BidirectionalCollection"):
    val a = 0x010203L
    assert(a.reversed.reversed.elementsEqual(a))

end BidirectionalCollectionTests

given Long is BidirectionalCollection:

  type Element = Byte
  type Position = Int

  extension (self: Long)

    final def start: Int =
      0

    final def end: Int =
      def loop(i: Int, a: Long): Int =
        if a == 0 then i else loop(i + 1, a >> 8)
      loop(0, self)

    final def positionAfter(p: Int): Int =
      p + 1

    final def positionBefore(p: Int): Int =
      p - 1

    final def isWithin(p: Int, low: Int, high: Int): Boolean =
      (p >= 0) && (p < high)

    final def apply(p: Int) =
      ((self >> (p * 8)) & 0xff).toByte

  end extension

end given
