package org.podval.judaica.viewer

object Orderer {

  def close[K](arcs: Map[K, Seq[K]]): Map[K, Set[K]] = close(arcs.keySet, arcs./*view.*/mapValues(_.toSet)/*.toMap*/)

  def close[K](roots: Set[K], arcs: K => Set[K]): Map[K, Set[K]] = {
    def close(root: K): Set[K] = {
      @scala.annotation.tailrec
      def close(acc: Set[K], next: Set[K]): Set[K] = {
        val add: Set[K] = next -- acc
        if (add.isEmpty) acc else close(acc ++ add, add flatMap arcs)
      }

      close(Set.empty, arcs(root))
    }

    roots.map(root => root -> close(root)).toMap
  }

  def inCycles[K](reachable: Map[K, Set[K]]): Set[K] =
    reachable.filter { case (k, c) => c.contains(k) }.keySet

  def order[K](reachable: Map[K, Set[K]]): Seq[K] = {
    @scala.annotation.tailrec
    def order(acc: Seq[K], left: Set[K]): Seq[K] = {
      if (left.isEmpty) acc else {
        val next = left.filter(reachable(_).subsetOf(acc.toSet))
        require(next.nonEmpty)
        order(acc ++ next, left -- next)
      }
    }

    order(Seq.empty, reachable.keys.toSet)
  }
}
