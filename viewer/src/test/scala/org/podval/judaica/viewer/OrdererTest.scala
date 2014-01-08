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

import org.junit.Test
import org.junit.Assert.{assertTrue, assertFalse, assertEquals}


final class OrdererTest {

  @Test
  def empty {
    val arcs: Map[String, Seq[String]] = Map.empty
    val reachable = Orderer.close(arcs)
    assertTrue(Orderer.inCycles(reachable).isEmpty)
    val result = Orderer.order(reachable)
    assertTrue(result.isEmpty)
  }


  @Test
  def one {
    val arcs: Map[String, Seq[String]] = Map("one" -> Seq.empty)
    val reachable = Orderer.close(arcs)
    assertTrue(Orderer.inCycles(reachable).isEmpty)
    assertEquals(Seq("one"), Orderer.order(reachable))
  }


  @Test
  def cycleOfOne {
    val arcs: Map[String, Seq[String]] = Map("one" -> Seq("one"))
    assertEquals(Set("one"), Orderer.inCycles(Orderer.close(arcs)))
  }


  @Test
  def two {
    val arcs: Map[String, Seq[String]] = Map("two" -> Seq("one"), "one" -> Seq.empty)
    val reachable = Orderer.close(arcs)
    assertTrue(Orderer.inCycles(reachable).isEmpty)
    assertEquals(Seq("one", "two"), Orderer.order(reachable))
  }


  @Test
  def cycleOfTwo {
    val arcs: Map[String, Seq[String]] = Map("two" -> Seq("one"), "one" -> Seq("two"))
    assertEquals(Set("one", "two"), Orderer.inCycles(Orderer.close(arcs)))
  }


  @Test
  def chumash {
    val arcs: Map[String, Seq[String]] = Map(
      "work" -> Seq("book"),
      "book" -> Seq("chapter", "week"),
      "chapter" -> Seq("verse"),
      "verse" -> Seq("word"),
      "week" -> Seq("day", "chapter"),
      "day" -> Seq("verse"),
      "word" -> Seq.empty
    )
    val reachable = Orderer.close(arcs)
    assertTrue(Orderer.inCycles(reachable).isEmpty)
    val result = Orderer.order(reachable)
    assertTrue(result.startsWith(Seq("word", "verse")))
    assertTrue(result.endsWith(Seq("book", "work")))
  }
}
