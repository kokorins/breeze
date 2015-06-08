package breeze.stats

import breeze.collection.immutable.CartesianTree
import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand
import breeze.stats.quantiles.{QuantileNode, QuantileTree, QuantileImpl, Quantile}

object quantile extends UFunc {
  type S = Double
  @expand
  implicit def canTraverseValuesImpl[T](implicit iter: CanTraverseValues[T, S]): Impl[T, Quantile[S]] = new Impl[T, Quantile[S]] {
    override def apply(v: T): Quantile[S] = {
      var quantileImpl = new QuantileImpl[S](new QuantileTree[S](1e-3, 0.0, 0.0))
      val visitor = new ValuesVisitor[S] {
        override def visit(a: S): Unit = {
          quantileImpl = quantileImpl.add(a)
        }

        override def zeros(numZero: Int, zeroValue: S): Unit = {
          for(i<-0 until numZero)
            quantileImpl = quantileImpl.add(zeroValue)
        }
      }
      iter.traverse(v, visitor)
      quantileImpl
    }
  }

}
