package breeze.stats

import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand

//object quantile extends UFunc {
//  @expand
//  implicit def canTraverseValuesImpl[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl[T, Quantile[S]] = new Impl[T, Quantile[S]] {
//
//    def apply(v: T): Quantile[S] = {
//      val visitor = new ValuesVisitor[S] {
//        def visit(a: S) = {
//        }
//
//        def zeros(numZero: Int, zeroValue: S): Unit = {
//        }
//      }
//      iter.traverse(v, visitor)
//      new QuantileImpl(new QuantileTree[S])
//    }
//  }
//
//  trait Quantile[S] {
//    def quant(x: S): S
//  }
//
//  class QuantileImpl[S](quantileTree: QuantileTree[S]) extends Quantile[S] {
//    def quant(x: S) = quantileTree.quantile(x)
//  }
//
//  case class QuantileNode[S](v: S, g: Int, d: Int)
//
//  class QuantileTree[S] {
//    def quantile(x: S) = ???
//
//    def insert(n: QuantileNode) = ???
//
//    def compress() = ???
//
//    def delete(n: QuantileNode) = ???
//  }
//
//}
