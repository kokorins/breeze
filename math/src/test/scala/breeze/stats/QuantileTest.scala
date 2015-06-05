package breeze.stats

import breeze.collection.immutable.CartesianTree
import breeze.stats.quantile.{QuantileNode, QuantileTree}
import breeze.stats.quantile.quantile.{QuantileImpl, QuantileSmImpl}
import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop.Checkers

import scala.util.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class quantileTest extends FunSuite with Checkers {
  test("Special case") {
    check( {
      val xs = List[Double](5.069837965422152E307, 1.068866138925466E307, -2.6050746577833916E307)
      val qSm = new QuantileSmImpl[Double](xs)
      val q = xs.foldLeft(new QuantileImpl[Double](new QuantileTree(1e-3, 0.0, 0.0)({q:QuantileNode[Double]=>q.value},Ordering[Double], Ordering.by{q:QuantileNode[Double]=>q.g+q.delta}))){ (q, x)=>
        q.add(x)
      }
      Math.abs(qSm.quant(0.5).getOrElse(0.0) - q.quant(0.5).getOrElse(0.0))<1e-2
    })
  }

  test("Quantile Tree") {
    check(Prop.forAll({ xs:List[Double] => {
      val xss = xs.take(Math.min(xs.size,1000))
      val qSm = new QuantileSmImpl[Double](xss)
      val q = xss.foldLeft(new QuantileImpl[Double](new QuantileTree(1e-3, 0.0, 0.0)({q:QuantileNode[Double]=>q.value},Ordering[Double], Ordering.by{q:QuantileNode[Double]=>q.g+q.delta}))){ (q, x)=>
        q.add(x)
      }
      Math.abs(qSm.quant(0.5).getOrElse(0.0) - q.quant(0.5).getOrElse(0.0))<1e-2
    }}))
  }

  test("Same values quantile") {
    check(Prop.forAll { (sz:Int, v:Double) => {
        val xs = {
          for (z <- 0 until Math.min(sz,1000)) yield v
        }
        val x = new QuantileSmImpl(xs.toList)
        val exp = if(sz>0) Success(v) else Failure(new Exception)
        val q = x.quant(0.5)
        if(exp.isFailure && q.isFailure)
          true
        else if(exp.isFailure || q.isFailure)
          false
        else
          Ordering[Double].compare(exp.get, q.get) == 0
        }
    })
  }
}
