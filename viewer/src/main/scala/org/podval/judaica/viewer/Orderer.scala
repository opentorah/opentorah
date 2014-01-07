/*
 * Copyright 2014 Leonid Dubinsky <dub@podval.org>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.judaica.viewer


object Orderer {

  // TODO is there a superclass for Set and Seq that has "--" - so that I can have one entry point here for both?


  def order[K](arcs: Map[K, Seq[K]]): Either[Set[K], Seq[K]] = orderSets(arcs.mapValues(_.toSet))


  def orderSets[K](arcs: Map[K, Set[K]]): Either[Set[K], Seq[K]] = order(arcs.keySet, arcs)


  def order[K](roots: Set[K], arcs: K => Set[K]): Either[Set[K], Seq[K]] = {

    def close(root: K): Set[K] = {
      def close(acc: Set[K], next: Set[K]): Set[K] = {
        val add: Set[K] = next -- acc
        if (add.isEmpty) acc else close(acc ++ add, add flatMap arcs)
      }

      close(Set.empty, arcs(root))
    }


    def order(reachable: Map[K, Set[K]]): Seq[K] = {
      def order(acc: Seq[K], left: Set[K]): Seq[K] = {
        if (left.isEmpty) acc else {
          val next = left.filter(reachable(_).subsetOf(acc.toSet))
          require(!next.isEmpty)
          order(acc ++ next, left -- next)
        }
      }

      order(Seq.empty, reachable.keys.toSet)
    }


    val reachable: Map[K, Set[K]] = roots.map(r => (r -> close(r))).toMap

    val cycled: Map[K, Set[K]] = reachable.filter { case (k, c) => c.contains(k) }

    val inCycles: Set[K] = cycled.map(_._1).toSet

    if (inCycles.isEmpty) Right(order(reachable)) else Left(inCycles)
  }
}
