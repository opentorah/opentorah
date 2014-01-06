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
import org.junit.Assert.{assertTrue, assertEquals}


final class OrdererTest {

  @Test
  def empty {
    val arcs: Map[String, Set[String]] = Map.empty
    val resultEither = Orderer.order(arcs)
    assertTrue(resultEither.isRight)
    val result = resultEither.right.get
    assertTrue(result.isEmpty)
  }


  @Test
  def one {
    val arcs: Map[String, Set[String]] = Map("one" -> Set.empty)
    val resultEither = Orderer.order(arcs)
    assertTrue(resultEither.isRight)
    val result = resultEither.right.get
    assertEquals(result, Seq("one"))
  }


  @Test
  def cycleOfOne {
    val arcs: Map[String, Set[String]] = Map("one" -> Set("one"))
    val resultEither = Orderer.order(arcs)
    assertTrue(resultEither.isLeft)
    val result = resultEither.left.get
    assertEquals(result, Set("one"))
  }


  @Test
  def two {
    val arcs: Map[String, Set[String]] = Map("two" -> Set("one"), "one" -> Set.empty)
    val resultEither = Orderer.order(arcs)
    assertTrue(resultEither.isRight)
    val result = resultEither.right.get
    assertEquals(result, Seq("one", "two"))
  }


  @Test
  def cycleOfTwo {
    val arcs: Map[String, Set[String]] = Map("two" -> Set("one"), "one" -> Set("two"))
    val resultEither = Orderer.order(arcs)
    assertTrue(resultEither.isLeft)
    val result = resultEither.left.get
    assertEquals(result, Set("one", "two"))
  }

  @Test
  def chumash {
    val arcs: Map[String, Set[String]] = Map(
      "work" -> Set("book"),
      "book" -> Set("chapter", "week"),
      "chapter" -> Set("verse"),
      "verse" -> Set("word"),
      "week" -> Set("day", "chapter"),
      "day" -> Set("verse"),
      "word" -> Set.empty
    )
    val resultEither = Orderer.order(arcs)
    assertTrue(resultEither.isRight)
    val result = resultEither.right.get
    assertTrue(result.startsWith(Seq("word", "verse")))
    assertTrue(result.endsWith(Seq("book", "work")))
  }
}
