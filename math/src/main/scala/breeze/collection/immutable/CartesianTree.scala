package breeze.collection.immutable

import breeze.stats.quantile

// Ordered immutable multimap
class CartesianTree[Value,Key](val root:Option[CartesianNode[Value, Key]])(implicit key:(Value=>Key), keyOrd:(Ordering[Key]), priority:Ordering[Value]) {
  def merge(that: Tree):Tree = {
    that.root.fold(this) { thatRoot =>
      add(thatRoot.v).merge(that.delete(thatRoot.v))
    }
  }

  type NonEmptyNode = CartesianNode[Value, Key]
  type Node = Option[CartesianNode[Value, Key]]
  type Tree = CartesianTree[Value, Key]

  def min() = CartesianIterator(root map {r=>r.min()})
  def max() = CartesianIterator(root map {r=>r.max()})
  def split(k: Key):(Tree, Tree, Tree) =
    root.fold((this, this, this)) { node => node.split(k) match {case (l,e,r) =>(CartesianTree(l), CartesianTree(e), CartesianTree(r))} }
  def add(value:Value):Tree =
    root.fold[Tree](CartesianTree(Some(CartesianNode[Value, Key](value, None, None)))){r=>CartesianTree(r.add(value))}
  def deleteByKey(k:Key) : Tree = root.fold[Tree](this)(r=>CartesianTree(r.deleteByKey(k)))
  def delete(v:Value):Tree = root.fold[Tree](this)(r=>CartesianTree(r.delete(v)))
  def removeSubTree(v:Value):Tree = root.fold[Tree](this)(r=>CartesianTree(r.removeSubTree(v)))
  def isEmpty = root.isEmpty
  def size = root.fold(0L)(r=>r.size)
  def fold[B](zero:B)(f:(Value,B)=>B)={
    root.fold(zero){r=>r.fold(zero)(f)}
  }
}

object CartesianTree {
  def apply[Value, Key]()(implicit key:(Value=>Key), keyOrd:Ordering[Key], priority:Ordering[Value]) = new CartesianTree[Value, Key](None)
  def apply[Value, Key](root: Option[CartesianNode[Value, Key]])(implicit key:(Value=>Key), keyOrd:Ordering[Key], priority:Ordering[Value]) = new CartesianTree[Value, Key](root)
}

class CartesianNode[Value, Key](val v:Value, val lhs:Option[CartesianNode[Value, Key]], val rhs:Option[CartesianNode[Value, Key]])(implicit key:(Value=>Key), keyOrd:(Key=>Ordered[Key]), priority:Ordering[Value]) {
  type NonEmptyNode = CartesianNode[Value, Key]
  type Node = Option[NonEmptyNode]

  def fold[B](zero:B)(f:(Value,B)=>B):B = {
    val cur = f(v, zero)
    val cl = lhs.fold[B](cur)(l=>l.fold[B](cur)(f))
    rhs.fold(cl)(r=>r.fold(cl)(f))
  }

  def min(parents:List[NonEmptyNode] = Nil):(NonEmptyNode, List[NonEmptyNode]) = lhs.fold((this, parents)){l=>l.min(this :: parents)}
  def max(parents:List[NonEmptyNode] = Nil):(NonEmptyNode, List[NonEmptyNode]) = rhs.fold((this, parents)){r=>r.max(this :: parents)}

  private def upUntil(pred:(NonEmptyNode, NonEmptyNode)=>Boolean)(parents:List[NonEmptyNode] = Nil):Option[(NonEmptyNode, List[NonEmptyNode])] = parents match {
    case Nil => None
    case h :: t => if(pred(this, h)) {
      Some((h,t))
    } else {
      h.upUntil(pred)(t)
    }
    
  }
  def predessesor(parents:List[NonEmptyNode] = Nil):Option[(NonEmptyNode, List[NonEmptyNode])] = lhs.fold(upUntil{(n, p)=> {p.lhs.fold(true){l=>l!=n}}}(parents)){l=>Some(l.max(this::parents))}
  
  def successor(parents:List[NonEmptyNode] = Nil):Option[(NonEmptyNode, List[NonEmptyNode])] = rhs.fold(upUntil((n, p)=>{p.rhs.fold(true)(r=>r!=n)})(parents)) { r=> Some(r.min(this::parents))}

  def delete(v: Value): Node = {
    val (l, e, r) = split(key(v))
    val newEq = DeleteRightExact(e, v)
    Meld(l, Meld(newEq, r))
  }

  def DeleteRightExact(eq:Node, v:Value) : Node = {
    eq.fold(eq){e=>
      if(e.v != v)
        Some(CartesianNode(e.v, e.lhs, DeleteRightExact(e.rhs, v)))
      else
        DeleteRightExact(e.rhs, v)
    }
  }

  def size:Long = fold(0L)((v,z)=>1+z)//lhs.fold(0L)(l=>l.size) + rhs.fold(0L)(r=>r.size) + 1L
  def split(k: Key)(implicit key:(Value=>Key), keyOrd:Ordering[Key], priority:Ordering[Value]): (Node, Node, Node) = {
    val thisKey = key(v)
    if(thisKey == k) { // all nodes with same key is on the right
      val (l, e, r) = rhs.fold[(Node, Node, Node)]((None, None, None))(_.split(k))
      (lhs, Meld(e, Some(CartesianNode(v, None, None))), r)
    } else if (keyOrd.lt(thisKey,k)) {
      val (l, e, r) = rhs.fold[(Node, Node, Node)]((None, None, None))(_.split(k))
      (Meld(Meld(lhs, Some(CartesianNode(v, None, None))),l), e, r)
    } else {
      val (l, e, r) = lhs.fold[(Node, Node, Node)]((None, None, None))(_.split(k))
      (l, e, Meld(r, Meld(Some(CartesianNode(v, None, None)), rhs)))
    }
  }

  def add(v:Value):Node = {
    val (l, e, r) = split(key(v))
    Meld(Meld(l, Meld(e, Some(CartesianNode(v, None, None)))),r)
  }

  def deleteByKey(k:Key):Node = {
    val (l, e, r) = split(k)
    Meld(l, r)
  }

  def removeSubTree(v:Value):Node = {
    val thisKey = key(v)
    val k = key(v)
    if(thisKey == k)
      None
    else if(thisKey<k) {
      Some(CartesianNode(v, lhs, rhs.fold[Node](None)(_.removeSubTree(v))))
    } else {
      Some(CartesianNode(v, lhs.fold[Node](None)(_.removeSubTree(v)), rhs))
    }
  }
}

object CartesianNode {
  def apply[Value, Key](v:Value, lhs:Option[CartesianNode[Value, Key]], rhs:Option[CartesianNode[Value, Key]])(implicit key:(Value=>Key), keyOrd:(Key=>Ordered[Key]), priority:Ordering[Value]) = new CartesianNode[Value, Key](v, lhs, rhs)
}

object Meld {
  def apply[Value, Key](lhs:CartesianTree[Value, Key], rhs:CartesianTree[Value, Key])(implicit key:(Value=>Key), keyOrd:(Key=>Ordered[Key]), priority:Ordering[Value]):CartesianTree[Value, Key] = {
    CartesianTree(Meld(lhs.root, rhs.root))
  }
  def apply[Value, Key](lhs:Option[CartesianNode[Value, Key]], rhs:Option[CartesianNode[Value, Key]])(implicit key:(Value=>Key), keyOrd:Ordering[Key], priority:Ordering[Value]):
            Option[CartesianNode[Value, Key]] = {
    lhs.fold(rhs) { l=>
      rhs.fold(lhs) { r=>
        if(key(l.v)==key(r.v)) {
          Some(CartesianNode(l.v, l.lhs, Meld(l.rhs, Some(r))))
        } else {
          if(priority.compare(l.v, r.v)>0) {
            Some(CartesianNode(l.v, l.lhs, Meld(l.rhs, Some(r))))
          }
          else {
            Some(CartesianNode(r.v, Meld(Some(l), r.lhs), r.rhs))
          }
        }
      }
    }
  }
}

class CartesianIterator[V,K](val node: CartesianNode[V,K], parents:List[CartesianNode[V,K]]){
  def this(p:(CartesianNode[V,K], List[CartesianNode[V,K]])) = this(p._1, p._2)
  def next(): Option[CartesianIterator[V,K]] = node.successor(parents) map {n=>new CartesianIterator[V,K](n)}
  def prev(): Option[CartesianIterator[V,K]] = node.predessesor(parents) map {n=>new CartesianIterator[V,K](n)}
}

object CartesianIterator {
//  def apply[V,K](root:Option[CartesianNode[V,K]]):Option[CartesianIterator[V,K]] = {
//    root.fold[Option[CartesianIterator[V,K]]](None){n=>Some(new CartesianIterator(n.min()))}
//  }
  def apply[V,K](pair:Option[(CartesianNode[V,K],List[CartesianNode[V,K]])]) = {
    pair.fold[Option[CartesianIterator[V,K]]](None){p=>Some(new CartesianIterator(p))}
  }
}
