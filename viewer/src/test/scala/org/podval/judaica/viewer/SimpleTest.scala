/*
 *  Copyright 2011-2014 Leonid Dubinsky <dub@podval.org>.
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

import org.scalatest.FlatSpec

// TODO switch to ScalaTest!
final class SimpleTest extends FlatSpec {

  "Works" should "find Хумаш" in {
    assertResult(true)(Works.workByName("Хумаш").isDefined)
  }

  it should "find Tanach" in {
    assertResult(true)(Works.workByName("Tanach").isDefined)
  }

  it should "get correct book selectors" in {
    val bookOption = Works.getWorkByName("Tanach").selectorByName("book")
    assertResult(true)(bookOption.isDefined)
    val book = bookOption.get
    assertResult(Set("chapter", "week"))(book.selectors.map(_.defaultName).toSet)
    assertResult(true)(book.isNumbered)
  }

  it should "get correct chapter selectors" in {
    val chapterOption = Works.getWorkByName("Tanach").selectorByName("book").get.selectorByName("chapter")
    assertResult(true)(chapterOption.isDefined)
    val chapter = chapterOption.get
    assertResult(Set("verse"))(chapter.selectors.map(_.defaultName).toSet)
    assertResult(true)(chapter.isNumbered)
  }

  it should "get correct default name" in {
    assertResult("Tanach")(Works.getWorkByName("Хумаш").defaultName)
  }

  it should "get correct directory" in {
    assertResult("Tanach")(Works.getWorkByName("Хумаш").directory.getName)
  }

  it should "find Toronto edition" in {
    assertResult(true)(Works.getWorkByName("Tanach").editionByName("Toronto").isDefined)
  }

  it should "find Genesis weeks structure" in {
    Selection("Chumash").selectPath("book/Genesis/week").asDiv.selectDiv("Genesis")
  }

  it should "get Chumash formats" ignore {
    val formats: Seq[Seq[Selector]] = Works.getWorkByName("Хумаш").formats
    formats.map(_.map(_.defaultName).mkString("/")).foreach(println)
  }

  it should "get Genesis formats" in {
    val selection = Selection("Хумаш").selectPath("book/Genesis").asStructure
    val formats: Seq[Seq[Selector]] = selection.div.formats
    formats.map(_.map(_.defaultName).mkString("/")).foreach(println)
  }

  it should "get Jerusalem edition storage" in {
    val storage = Works.getWorkByName("Tanach").getEditionByName("Jerusalem").storage
    assertResult(true)(storage.isDirectory)
    assertResult("book")(storage.asDirectory.structure.selector.defaultName)
    assertResult(5)(storage.asDirectory.storage.size)
  }

  it should "get Chumash content" in {
    Selection("Tanach", "Jerusalem").content(None)
  }

  it should "get Genesis content" in {
    Selection("Tanach", "Jerusalem").selectPath("book/Genesis").asStructure.content(None)
  }

  it should "get Genesis 1 content" in {
    val result = Selection("Tanach", "Jerusalem").selectPath("book/Genesis/chapter/1").asStructure.content(None)
    Xml.print(Content.toXmlNode(result), System.out)
  }

  it should "get Genesis 1:1 content" in {
    val result = Selection("Tanach", "Jerusalem").selectPath("book/Genesis/chapter/1/verse/1").asStructure.content(None)
    Xml.print(Content.toXmlNode(result), System.out)
  }
}
