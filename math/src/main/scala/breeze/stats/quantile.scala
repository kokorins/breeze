package breeze.stats

import breeze.generic.UFunc
import breeze.linalg.DenseVector
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand
import breeze.stats.quantiles.QuantileTree
import breeze.stats.quantiles.quantileImpl.{QuantileImpl, Quantile}

object quantile extends UFunc {
//  @expand
//  implicit def canTraverseValuesImpl[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl3[T, Int, (Double,Double), Histogram[S]] = new Impl3[T, Int, (Double,Double), Histogram[S]] {
  type S = Double
  implicit def canTraverseValuesImpl[T](implicit iter: CanTraverseValues[T, S]): Impl2[T, Double, Quantile[S]] = new Impl2[T, Double, Quantile[S]] {

  def apply(v: T, precision: Double): Quantile[S] = {
      var result = new QuantileImpl[S](new QuantileTree[S](precision, 0.0, 0.0))
      val visitor = new ValuesVisitor[S] {
        def visit(a: S) = {
          result = result.add(a)
        }
        def zeros(numZero: Int, zeroValue: S): Unit = {
          for(i<-0 until numZero)
            result = result.add(zeroValue)
        }
      }
      iter.traverse(v, visitor)
      result
    }
  }
}
