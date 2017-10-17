package brainysmith

import scala.annotation.tailrec
import scala.collection.mutable.Stack

final case class Edge[+V](val prefix: String, node: Node[V])
sealed trait Node[+V] { val isLeaf: Boolean }
final case class MiddleNode[V](val edges: Array[Edge[V]]) extends Node[V] { val isLeaf = false }
final case class LeafNode[V](val value: V) extends Node[V] { val isLeaf = true }

class RadixTree[V](private val root: MiddleNode[V]) extends Map[String, V] {

  override def get(key: String): Option[V] = lookup(key)

  override def iterator: Iterator[(String, V)] = new RadixIterator[V](root)

  override def + [V1 >: V](kv: (String, V1)): RadixTree[V1] = insert(kv._1, kv._2)

  override def -(key: String): RadixTree[V] = remove(key)

  override def empty: RadixTree[V] = RadixTree.empty


  def insert[V1 >: V](key: String, value: V1): RadixTree[V1] = new RadixTree(_update(key, value, root))

  def lookup(key: String): Option[V] = _search(root.edges, key)

  def findAllWithPrefix(prefix: String): Seq[(String, V)] = _searchWithPrefix(root.edges, prefix, prefix)

  def foldDepth[A](acc: A)(op: (A, (String, Option[V])) => A) = _depthFirstTravers(root.edges)(acc)(op)

  def foldBreadth[A](acc: A)(op: (A, (String, Option[V])) => A) = _breadthFirstTravers(root.edges)(acc)(op)

  def remove(key: String): RadixTree[V] = new RadixTree(_delete(key, root))


  @inline private def _findPosStringStartDiffer(a: String, b: String): Option[Int] = (0 until (a.length max b.length)).find(i => a.lift(i) != b.lift(i))

  @inline private def _matched(edges: Array[Edge[V]], key: String): Option[Int] = {
    val idx = edges.indexWhere(e => (key.isEmpty && e.prefix.isEmpty) || e.prefix.lift(0) == key.lift(0))
    if(idx == -1) None else Some(idx)
  }


  private def _update[V1 >: V](key: String, value: V1, root: MiddleNode[V]): MiddleNode[V1] = {
    val idx = _matched(root.edges, key)
    val node = idx.fold{
      val na = new Array[Edge[V1]](root.edges.length + 1)
      root.edges.copyToArray(na)
    MiddleNode(na)}{_ => 
      val na = new Array[Edge[V1]](root.edges.length)
      Array.copy(root.edges, 0, na, 0, root.edges.length)
      MiddleNode(na)
    }
    __update(key, value, node, idx, node)
  }

  @tailrec private def __update[V1 >: V](key: String, value: V1, node: MiddleNode[V1], optIdx: Option[Int], root: MiddleNode[V1]): MiddleNode[V1] = {
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
          val nn = MiddleNode[V1](i.fold(n.edges.clone() :+ Edge("", LeafNode(value)))(i => n.edges.updated(i, Edge("", LeafNode(value)))))
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
            val na = new Array[Edge[V1]](n.edges.length + 1)
            n.edges.copyToArray(na)
          MiddleNode(na)}{_ => 
            val na = new Array[Edge[V1]](n.edges.length)
            Array.copy(n.edges, 0, na, 0, n.edges.length)
            MiddleNode(na)
          }
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

  private def _excludeElem(src: Array[Edge[V]], idx: Int): Array[Edge[V]] = {
    val len = src.length
    val dst = new Array[Edge[V]](len - 1)
    Array.copy(src, 0, dst, 0, idx)
    Array.copy(src, idx + 1, dst, idx, len - idx - 1)
    dst
  }

  private def _delete(key: String, root: MiddleNode[V]): MiddleNode[V] = {
    val idx = root.edges.indexWhere(e => (key.isEmpty && e.prefix.isEmpty) || (e.prefix.nonEmpty && key.startsWith(e.prefix)))
    if(idx == -1) {
      root
    } else {
      root.edges(idx) match {
        case Edge(p, LeafNode(_)) if key == p => MiddleNode(_excludeElem(root.edges, idx))
        case Edge(p, LeafNode(_)) => root
        case Edge(p, MiddleNode(es)) => 
          val mn = MiddleNode(root.edges.clone())
          __delete(key.drop(p.length), p, es, mn, idx, mn)
      }
    }
  }

  @tailrec private def __delete(key: String, prefix: String, edges: Array[Edge[V]], mn: MiddleNode[V], i: Int, root:  MiddleNode[V]): MiddleNode[V] = {
    val idx = edges.indexWhere(e => (key.isEmpty && e.prefix.isEmpty) || (e.prefix.nonEmpty && key.startsWith(e.prefix)))
    if(idx == -1) {
      root
    } else {
      edges(idx) match {
        case Edge(p, LeafNode(_)) if key == p => 
          if(edges.size == 2) {
            val leftEdge = edges((idx + 1) & 0x01)
            val parentEdge = mn.edges(i)
            mn.edges(i) = Edge(parentEdge.prefix + leftEdge.prefix, leftEdge.node)
          } else if(edges.size > 2) {
            val n = MiddleNode(_excludeElem(edges, idx))
            mn.edges(i) = Edge(prefix, n)
          } else {
            throw new IllegalStateException("Unbalanced tree")
          }
          root
        case Edge(p, LeafNode(_)) => root
        case Edge(p, MiddleNode(es)) => 
          val n = MiddleNode(edges.clone())
          mn.edges(i) = Edge(prefix, n)
          __delete(key.drop(p.length), p, es, n, idx, root)
      }
    }
  }

  @tailrec private def _search(edges: Array[Edge[V]], key: String): Option[V] = {
    edges.find(e => (key.isEmpty && e.prefix.isEmpty) || (e.prefix.nonEmpty && key.startsWith(e.prefix))) match {
      case None => None
      case Some(Edge(prefix, LeafNode(value))) if key == prefix => Some(value)
      case Some(Edge(prefix, LeafNode(value))) => None
      case Some(Edge(prefix, MiddleNode(es))) => _search(es, key.drop(prefix.length))
    }
  }

  @tailrec private def _searchWithPrefix(edges: Array[Edge[V]], prefix: String, fullPrefix: String): Seq[(String, V)] = {
    if(prefix.isEmpty) {
      _depthFirstTravers(edges)(Seq[(String, V)]()){case (a, (p, Some(b))) => a :+ (fullPrefix + p -> b)
      case (a, (_, None)) => a}
    } else {
      _matched(edges, prefix) match {
        case None => Seq[(String, V)]()
        case Some(p) => edges(p) match {
          case Edge(p, MiddleNode(es)) if p == prefix || p.startsWith(prefix) => _depthFirstTravers(es)(Seq[(String, V)]()){case (a, (p, Some(b))) => a :+ (fullPrefix + p -> b)
            case (a, (_, None)) => a}
            case Edge(p, LeafNode(v)) if p == prefix || p.startsWith(prefix) => Seq(fullPrefix + p -> v)
          case Edge(p, MiddleNode(es)) if prefix.startsWith(p) => _searchWithPrefix(es, prefix.drop(p.length), fullPrefix)
          case Edge(p, LeafNode(_)) if prefix.startsWith(p) => Seq[(String, V)]()
        }
      }
    }
  }

  @inline private def _depthFirstTravers[A](rte: Array[Edge[V]])(acc: A)(op: (A, (String, Option[V])) => A) = {
    @tailrec def __depth(nodes: List[(String, Edge[V])], a: A): A = nodes match {
      case Nil => a
      case (parent, Edge(p, LeafNode(value))) :: tail => 
        __depth(tail, op(a, (parent + p, Some(value))))
      case (parent, Edge(p, MiddleNode(edges))) :: tail => 
        __depth(edges.map(v => ((parent + p) -> v)).toList ::: tail, op(a, (parent + p, None)))
    }
    __depth(rte.map(v => ("" -> v)).toList, acc)
  }

  @inline private def _breadthFirstTravers[A](rte: Array[Edge[V]])(acc: A)(op: (A, (String, Option[V])) => A) = {
    @tailrec def __depth(nodes: List[(String, Edge[V])], a: A): A = nodes match {
      case Nil => a
      case (parent, Edge(p, LeafNode(value))) :: tail => 
        __depth(tail, op(a, (parent + p, Some(value))))
      case (parent, Edge(p, MiddleNode(edges))) :: tail => 
        __depth(tail ::: edges.map(v => ((parent + p) -> v)).toList, op(a, (parent + p, None)))
    }
    __depth(rte.map(v => ("" -> v)).toList, acc)
  }

  class RadixIterator[V](private val root: MiddleNode[V]) extends Iterator[(String, V)] {
    private var stack: List[(String, Edge[V])] = List(root.edges.map(v => "" -> v): _*)
    private var value: Option[(String, V)] = _advance()

    @tailrec private def _advance(): Option[(String, V)] = {
      stack match {
        case Nil => None
        case (parent, Edge(p, LeafNode(v))) :: tail => 
          stack = tail
          Some((parent + p, v))
        case (parent, Edge(p, MiddleNode(edges))) :: tail => 
          stack = edges.map(v => ((parent + p) -> v)).toList ::: tail
          _advance()
      }
    }

    override def hasNext: Boolean = value.nonEmpty
    override def next(): (String, V) = {
      val res = value.get
      if(value.nonEmpty) value = _advance()
      res
    }
  }

}


object RadixTree {

  def apply[V](pairs: (String, V)*): RadixTree[V] = pairs.foldLeft(empty[V])((a, b) => a.insert(b._1, b._2))

  def empty[V]: RadixTree[V] = new RadixTree(MiddleNode[V](Array.empty))

}
