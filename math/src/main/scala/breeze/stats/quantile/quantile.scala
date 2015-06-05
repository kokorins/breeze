package breeze.stats.quantile

import breeze.collection.immutable.CartesianTree

import scala.util.Try

object quantile {

  trait Quantile[S] {
    def quant(x: Double): Try[S]
    def add(x:S):Quantile[S]
  }

  class QuantileSmImpl[S](values: List[S])(implicit ordering: Ordering[S]) extends Quantile[S] {
    def quant(x:Double):Try[S] = {
      val idx = Math.round(x*(values.size-1)).toInt
      Try(values.sorted.take(Math.round(x*values.size).toInt).head)
    }
    def add(x:S) = new QuantileSmImpl(x::values)
  }

  class QuantileImpl[S](quantileTree: QuantileTree[S]) extends Quantile[S] {
    def quant(x: Double):Try[S] = quantileTree.quantile(x)
    def add(x:S) = new QuantileImpl(quantileTree.insert(x))
  }
}
