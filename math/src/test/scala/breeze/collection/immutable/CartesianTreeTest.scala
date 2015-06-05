package breeze.collection.immutable

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

import scala._

@RunWith(classOf[JUnitRunner])
class CartesianTreeTest extends FunSuite with Checkers {
  test("Special") {
    check {
      val xs = List(0, 1, -1)
      var t = CartesianTree[Int, Int]()(key = { k => k }, keyOrd = { x => x }, priority =  Ordering.by { x:Int =>
        val h = x.hashCode()
        ((h << 16) & 0xffff0000) | ((h >> 16) & 0x0000ffff)
      })
      val distinct = xs.groupBy(x => x).keySet.toList
      distinct foreach { x => t = t.add(x) }
      var it = CartesianIterator(t.root)
      for (x <- distinct.sorted) {
        it.fold(fail("Not enough values in a treap")) { i =>
          if (x != i.node.v)
            fail(x.toString + " is not equal to " + i.node.v.toString)
          it = i.next()
        }
      }
      it.fold() { x =>
        fail("Some additional values")
      }
      true
    }
  }
  test("Sorted distinct values") {
    check(Prop.forAll { xs: List[Int] => {
      var t = CartesianTree[Int, Int]()(key = { k => k }, keyOrd = { x => x }, priority =  Ordering.by { x:Int =>
        val h = x.hashCode()
        ((h << 16) & 0xffff0000) | ((h >> 16) & 0x0000ffff)
      })
      val distinct = xs.groupBy(x => x).keySet.toList
      distinct foreach { x => t = t.add(x) }
      var it = CartesianIterator(t.root)
      for (x <- distinct.sorted) {
        it.fold(fail("Not enough values in a treap")) { i =>
          if (x != i.node.v)
            fail(x.toString + " is not equal to " + i.node.v.toString)
          it = i.next()
        }
      }
      it.fold() { x => fail("Some additional values") }
      true
    }
    })
  }

  test("All values should be applied") {
    check(Prop.forAll { xs: List[Int] => {
      var t = CartesianTree[Int, Int]()(key = { k => k }, keyOrd = { x => x }, priority =  Ordering.by { x:Int =>
        val h = x.hashCode()
        ((h << 16) & 0xffff0000) | ((h >> 16) & 0x0000ffff)
      })

      xs foreach { x => t = t.add(x) }
      var it = CartesianIterator(t.root)
      for (x <- xs.sorted) {
        it.fold(fail("Not enough values in a treap")) { i =>
          if (x != i.node.v)
            fail(x.toString + " is not equal to " + i.node.v.toString)
          it = i.next()
        }
      }
      it.fold() { x => fail("Some additional values") }
      true
    }
    })
  }

}
