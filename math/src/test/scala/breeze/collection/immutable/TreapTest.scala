package breeze.collection.immutable

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TreapTest extends FunSuite with Checkers {
  test("Sorted distinct values") {
    check(Prop.forAll {
      xs:List[Int] => {
        var t = Treap[Int, Double]()
        val distinct = xs.groupBy(x=>x).keySet.toList
        distinct foreach {x=> t = t.upd(x, Random.nextDouble())}
        (distinct.sorted zip t.keys) forall {case (exp, v) => exp.equals(v)}
      }
    })
  }

}
