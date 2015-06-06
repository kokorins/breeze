package breeze.stats.quantile

import breeze.collection.immutable.{CartesianIterator, CartesianTree}

import scala.util.Try

case class QuantileNode[S](value: S, g: Long, delta: Long)

class QuantileTree[S](precision: Double, val treap: CartesianTree[QuantileNode[S], S], val min: S, val max: S)(implicit keyOrd: Ordering[S]) {
  implicit object key extends (QuantileNode[S]=>S) {
    def apply(quantileNode: QuantileNode[S]):S = quantileNode.value
  }
  implicit object priority extends Ordering[QuantileNode[S]] {
    override def compare(x: QuantileNode[S], y: QuantileNode[S]): Int = Ordering[Long].compare(x.g+x.delta, y.g+y.delta)
  }

  def this(precision:Double, minL:S, maxL:S)(implicit key:(QuantileNode[S]=>S), keyOrd: Ordering[S], priority:Ordering[QuantileNode[S]]) = this(precision, CartesianTree[QuantileNode[S], S](), minL, maxL)

  def merge(quantileTree: QuantileTree[S]) = {
    val t = treap.merge(quantileTree.treap)
    new QuantileTree[S](precision, t)
  }

  def quantile(x: Double): Try[S] = {
    val sz = treap.size
    val r = Math.round(x * sz)
    val l = precision * sz
    Try {
      var it = treap.min()
      var (rmin, rmax) = (0L, 0L)
      var v: Option[S] = None
      while (it.isDefined) {
        it.fold() { i =>
          val n = i.node.v
          rmin = rmin + n.g
          rmax = rmin + n.delta
          if (v.isEmpty && r - rmin <= l && rmax - r <= l)
            v = Some(n.value)
        }
        it = it.get.next()
      }
      v.fold(throw new Exception()) { g => g }
    }
  }

  def insert(x: S) = {
    val (locMin, locMax) = if(treap.isEmpty)(x, x) else (keyOrd.min(min,x), keyOrd.max(max,x))
    val sz = treap.size
    val d = if (keyOrd.lteq(x, min) || keyOrd.gteq(x, max)) 0 else Math.round(Math.floor(2 * precision * sz))
    val qt = new QuantileTree[S](precision, treap.add(QuantileNode(x, 1, d)), locMin, locMax)
    if ((sz + 1) % Math.round(1 / (2 * precision)) == 0)
      qt.compress()
    else
      qt
  }

  def compress() = {
    var locTreap = treap
    val eps = 2*precision*locTreap.size
    var next = locTreap.max()
    if(next.isEmpty)
      new QuantileTree[S](precision, locTreap, min, max)
    else {
      while(next.isDefined) {
        next.fold() { n => {
          val cur = n.prev()
          cur.fold() { c =>
            var g = c.node.fold(0L){(v,z)=>v.g+z}
            if (Band(c.node.v, eps) <= Band(n.node.v, eps) && g + n.node.v.g + n.node.v.delta < eps) {
              var it = CartesianIterator(Some(c.node, Nil))
              while(it.isDefined){
                it.fold(){i=>
                  locTreap = locTreap.delete(i.node.v)
                  it = i.next()
                }
              }
            }
            g = g - n.node.v.g
            next = cur
          }
        }
        }
      }
      new QuantileTree[S](precision, locTreap, min, max)
    }

  }

  def Band(b:QuantileNode[S], eps:Double) = b.delta

  def delete(node: QuantileNode[S]) = {
    new QuantileTree(precision, treap.delete(node), min, max)
  }

}