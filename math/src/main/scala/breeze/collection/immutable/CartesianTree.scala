package breeze.collection.immutable

class CartesianTree[V](root:DecartesNode[V]) {

}

case class DecartesNode[V](v:V, lhs:Option[DecartesNode[V]], rhs:Option[DecartesNode[V]])(implicit key:Ordering[V], priority:Ordering[V])
