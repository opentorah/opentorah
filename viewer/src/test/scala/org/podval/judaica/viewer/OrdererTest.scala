/*
 * Copyright 2014-2020 Leonid Dubinsky <dub@podval.org>.
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

import org.scalatest.flatspec.AnyFlatSpec

final class OrdererTest extends AnyFlatSpec {

  "Orderer" should "handle empty correctly" in {
    val arcs: Map[String, Seq[String]] = Map.empty
    val reachable = Orderer.close(arcs)
    assertResult(true)(Orderer.inCycles(reachable).isEmpty)
    val result = Orderer.order(reachable)
    assertResult(true)(result.isEmpty)
  }

  it should "handle one correctly" in {
    val arcs: Map[String, Seq[String]] = Map("one" -> Seq.empty)
    val reachable = Orderer.close(arcs)
    assertResult(true)(Orderer.inCycles(reachable).isEmpty)
    assertResult(Seq("one"))(Orderer.order(reachable))
  }

  it should "handle cycle of one correctly" in {
    val arcs: Map[String, Seq[String]] = Map("one" -> Seq("one"))
    assertResult(Set("one"))(Orderer.inCycles(Orderer.close(arcs)))
  }

  it should "handle two correctly" in {
    val arcs: Map[String, Seq[String]] = Map("two" -> Seq("one"), "one" -> Seq.empty)
    val reachable = Orderer.close(arcs)
    assertResult(true)(Orderer.inCycles(reachable).isEmpty)
    assertResult(Seq("one", "two"))(Orderer.order(reachable))
  }

  it should "handle cycle of two correctly" in {
    val arcs: Map[String, Seq[String]] = Map("two" -> Seq("one"), "one" -> Seq("two"))
    assertResult(Set("one", "two"))(Orderer.inCycles(Orderer.close(arcs)))
  }

  it should "handle Chumash correctly" in {
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
    assertResult(true)(Orderer.inCycles(reachable).isEmpty)
    val result = Orderer.order(reachable)
    assertResult(true)(result.startsWith(Seq("word", "verse")))
    assertResult(true)(result.endsWith(Seq("book", "work")))
  }
}
