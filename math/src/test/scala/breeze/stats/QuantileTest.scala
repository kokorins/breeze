package breeze.stats

import breeze.stats.quantile.quantile.QuantileSmImpl
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
      val xs = List[Double](1)
      val x = new QuantileSmImpl(xs)
      val exp = Success(1.0)
      val q = x.quant(0.5)
      if(exp.isFailure && q.isFailure)
        true
      else if(exp.isFailure || q.isFailure)
        false
      else
        Ordering[Double].compare(exp.get, q.get) == 0
    })
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
