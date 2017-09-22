package brainysmith

import scala.annotation.tailrec

final case class Edge[V](val prefix: String, node: Node[V])
sealed trait Node[V] { val isLeaf: Boolean }
final case class MiddleNode[V](val edges: Array[Edge[V]]) extends Node[V] { val isLeaf = false }
final case class LeafNode[V](val value: V) extends Node[V] { val isLeaf = true }

class RadixTree[V](private val root: MiddleNode[V]) {

  def insert(key: String, value: V): RadixTree[V] = new RadixTree(_update(key, value, root))

  def lookup(key: String): Option[V] = _search(root.edges, key)

  def findAllWithPrefix(prefix: String): Seq[V] = _searchWithPrefix(root.edges, prefix)

  def foldDepth[A](acc: A)(op: (A, (String, Option[V])) => A) = _depthFirstTravers(root.edges)(acc)(op)

  def foldBreadth[A](acc: A)(op: (A, (String, Option[V])) => A) = _breadthFirstTravers(root.edges)(acc)(op)

  //def remove(key: String): RadixTree[V] = ???



  @inline private def _findPosStringStartDiffer(a: String, b: String): Option[Int] = (0 until (a.length max b.length)).find(i => a.lift(i) != b.lift(i))

  @inline private def _matched(edges: Array[Edge[V]], key: String): Option[Int] = {
    val idx = edges.indexWhere(e => (key.isEmpty && e.prefix.isEmpty) || e.prefix.lift(0) == key.lift(0))
    if(idx == -1) None else Some(idx)
  }

  private def _update(key: String, value: V, root: MiddleNode[V]): MiddleNode[V] = {
    val idx = _matched(root.edges, key)
    val node = idx.fold{
      val na = new Array[Edge[V]](root.edges.length + 1)
      root.edges.copyToArray(na)
    MiddleNode(na)}(_ => MiddleNode(root.edges.clone()))
    __update(key, value, node, idx, node)
  }

  @tailrec private def __update(key: String, value: V, node: MiddleNode[V], optIdx: Option[Int], root: MiddleNode[V]): MiddleNode[V] = {
    if(optIdx.isEmpty) {
      node.edges(node.edges.length - 1) = Edge(key, LeafNode(value))
      root
    } else { 
      val idx = optIdx.get
      val edge = root.edges(idx)
      _findPosStringStartDiffer(key, edge.prefix) match {
        case None if edge.node.isLeaf => 
          node.edges(idx) = edge.copy(node = LeafNode(value))
          root
        case None =>
          val n = edge.node.asInstanceOf[MiddleNode[V]]
          val i = _matched(n.edges, "")
          val nn = MiddleNode[V](i.fold(n.edges.clone() :+ Edge("", LeafNode(value)))(i => n.edges.updated(i, Edge("", LeafNode(value)))))
          node.edges(idx) = edge.copy(node = nn)
          root
        case Some(p) if p == edge.prefix.length && edge.node.isLeaf => //Exact prefix match
          val prefix = key.substring(0, p)
          val suffix = key.substring(p)
          val nn = MiddleNode(Array(Edge("", edge.node), Edge(suffix, LeafNode(value))))
          node.edges(idx) = Edge(prefix, nn)
          root
        case Some(p) if p == edge.prefix.length => //Exact prefix match
          val suffix = key.substring(p)
          val n = edge.node.asInstanceOf[MiddleNode[V]]
          val i = _matched(n.edges, suffix)
          val nn = i.fold{
            val na = new Array[Edge[V]](n.edges.length + 1)
            n.edges.copyToArray(na)
          MiddleNode(na)}(_ => MiddleNode(n.edges.clone()))
          node.edges(idx) = edge.copy(node = nn)
          __update(suffix, value, nn, i, root)
        case Some(p) => //Empty edge and common prefix
          val commonPrefix = key.substring(0, p)
          val kSuffix = key.substring(p)
          val eSuffix = edge.prefix.substring(p)
          val nn = MiddleNode(Array(Edge(eSuffix, edge.node), Edge(kSuffix, LeafNode(value))))
          node.edges(idx) = Edge(commonPrefix, nn)
          root
      }
    }
  }

  /*private def _delete(key: String, root: MiddleNode[V]): MiddleNode[V] = {
    val idx = root.edges.indexWhere(e => (key.isEmpty && e.prefix.isEmpty) || (e.prefix.nonEmpty && key.startsWith(e.prefix)))
    if(idx = -1) {
      root
    } else {
      root.edges(idx) match {
        case Edge(p, LeafNode(_)) if key == p => MiddleNode(root.edges.)
      }
    }
  }*/


  @tailrec private def _search(edges: Array[Edge[V]], key: String): Option[V] = {
    edges.find(e => (key.isEmpty && e.prefix.isEmpty) || (e.prefix.nonEmpty && key.startsWith(e.prefix))) match {
      case None => None
      case Some(Edge(prefix, LeafNode(value))) if key == prefix => Some(value)
      case Some(Edge(prefix, LeafNode(value))) => None
      case Some(Edge(prefix, MiddleNode(es))) => _search(es, key.drop(prefix.length))
    }
  }

  @tailrec private def _searchWithPrefix(edges: Array[Edge[V]], prefix: String): Seq[V] = {
    if(prefix.isEmpty) {
      _depthFirstTravers(edges)(Seq[V]()){case (a, (_, Some(b))) => a :+ b
      case (a, (_, None)) => a}
    } else {
      _matched(edges, prefix) match {
        case None => Seq[V]()
        case Some(p) => edges(p) match {
          case Edge(p, MiddleNode(es)) if p == prefix || p.startsWith(prefix) => _depthFirstTravers(es)(Seq[V]()){case (a, (_, Some(b))) => a :+ b
            case (a, (_, None)) => a}
          case Edge(p, LeafNode(v)) if p == prefix || p.startsWith(prefix) => Seq(v)
          case Edge(p, MiddleNode(es)) if prefix.startsWith(p) => _searchWithPrefix(es, prefix.drop(p.length))
          case Edge(p, LeafNode(_)) if prefix.startsWith(p) => Seq[V]()
        }
      }
    }
  }

  @inline private def _depthFirstTravers[A](rte: Array[Edge[V]])(acc: A)(op: (A, (String, Option[V])) => A) = {
    @tailrec def __depth(nodes: List[Edge[V]], a: A): A = nodes match {
      case Nil => a
      case Edge(prefix, LeafNode(value)) :: tail => 
        __depth(tail, op(a, (prefix, Some(value))))
      case Edge(prefix, MiddleNode(edges)) :: tail => 
        __depth(edges.toList ::: tail, op(a, (prefix, None)))
    }
    __depth(rte.toList, acc)
  }

  @inline private def _breadthFirstTravers[A](rte: Array[Edge[V]])(acc: A)(op: (A, (String, Option[V])) => A) = {
    @tailrec def __depth(nodes: List[Edge[V]], a: A): A = nodes match {
      case Nil => a
      case Edge(prefix, LeafNode(value)) :: tail => 
        __depth(tail, op(a, (prefix, Some(value))))
      case Edge(prefix, MiddleNode(edges)) :: tail => 
        __depth(tail ::: edges.toList, op(a, (prefix, None)))
    }
    __depth(rte.toList, acc)
  }

}


object RadixTree {

  def apply[V](pairs: (String, V)*): RadixTree[V] = pairs.foldLeft(empty[V])((a, b) => a.insert(b._1, b._2))

  def empty[V]: RadixTree[V] = new RadixTree(MiddleNode[V](Array.empty))

}
