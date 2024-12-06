// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import language.experimental.captureChecking
import language.experimental.modularity

import mut.{Vector, Collection}
import mut.given

class CollectionTests extends munit.FunSuite:

  test("positionOffsetBy"):
    val a = 0x010203L
    assertEquals(a.positionOffsetBy(0, 2), 2)

  test("at"):
    val a = 0x010203L
    assertEquals(a.at(1), 2.toByte)

  test("count"):
    val a = 0x010203L
    assertEquals(a.count, 3)

  test("isEmpty"):
    assert(0L.isEmpty)
    assertEquals(1L.isEmpty, false)

  test("first"):
    assertEquals(0L.first, None)
    assertEquals(0x010203L.first, Some(3.toByte))

  test("firstWhere"):
    val a = 0x010203L
    assertEquals(a.firstWhere(_ < 3), Some(2.toByte))
    assertEquals(a.firstWhere(_ > 5), None)

  test("forEach"):
    val a = 0x010203L
    var b = 0L
    var i = 0
    a.forEach { (e) => b += (e << i); i += 8 }
    assertEquals(a, b)

  test("reduce"):
    val a = 0x010203L
    assertEquals(a.reduce("") { (s, e) => s + e.toString }, "321")

  test("reduceInto"):
    val a = 0x010203L
    val s = a.reduce(new java.lang.StringBuilder()) { (s, e) => s.append(e.toString) }
    assertEquals(s.toString, "321")

  test("map"):
    val a = 0x010203L
    val m = a.map(_ + 1).equals(Vector(4, 3, 2))

  test("filter"):
    val a = 0x010203L
    val m = a.filter(_ % 2 == 0).equals(Vector(2))

  test("min"):
    val a = 0x010203L
    assertEquals(a.min, Some(1.toByte))
    val b = 0L
    assertEquals(b.min, None)

  test("max"):
    val a = 0x010203L
    assertEquals(a.max, Some(3.toByte))
    val b = 0L
    assertEquals(b.max, None)

  test("firstPositionWhere"):
    val a = 0x010203L
    assertEquals(a.firstPositionWhere(_ < 3), Some(1))
    assertEquals(a.firstPositionWhere(_ > 5), None)

  test("firstPositionOf"):
    val a = 0x010203L
    assertEquals(a.firstPositionOf(2), Some(1))
    assertEquals(a.firstPositionOf(5), None)

  test("containsWhere"):
    val a = 0x010203L
    assert(a.containsWhere(_ < 3))
    assertEquals(a.containsWhere(_ > 5), false)

  test("contains"):
    val a = 0x010203L
    assert(a.contains(2))
    assertEquals(a.contains(5), false)

  test("allSatisfy"):
    val a = 0x010203L
    assert(a.allSatisfy(_ > 0))
    assertEquals(a.allSatisfy(_ > 1), false)
    assert(0L.allSatisfy { _ => false })

  test("elementsEqualBy"):
    val a = 0x010203L
    assert(a.elementsEqualBy(a)(_ == _))
    val b = 0x030201L
    assertEquals(a.elementsEqualBy(b)(_ == _), false)

  test("elementsEqual"):
    val a = 0x010203L
    assert(a.elementsEqual(a))
    val b = 0x030201L
    assertEquals(a.elementsEqual(b), false)

  test("Slice.equals"):
    val a = 0x010203L
    a.withSlice(0, 2) { (s) =>
      assert(a.withSlice(0, 2) { (t) => s == t })
      assert(a.withSlice(1, 2) { (t) => s != t })
    }

  test("Slice.hashCode"):
    val a = 0x010203L
    a.withSlice(0, 2) { (s) =>
      assert(a.withSlice(0, 2) { (t) => s.hashCode == t.hashCode })
      assert(a.withSlice(1, 3) { (t) => s.hashCode != t.hashCode })
    }

  test("Slice is Collection"):
    val a = 0x010203L
    assert(a.withSlice(0, 2) { (s) => s.elementsEqual(0x0203L) })

end CollectionTests

given Long is Collection:

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

    final def isWithin(p: Int, low: Int, high: Int): Boolean =
      (p >= 0) && (p < high)

    final def apply(p: Int) =
      ((self >> (p * 8)) & 0xff).toByte

  end extension

end given
