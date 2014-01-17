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

import org.junit.Test
import org.junit.Assert.{assertTrue, assertFalse, assertEquals}


// TODO switch to ScalaTest!
class SimpleTest {

  @Test
  def findChumash = assertTrue(Works.workByName("Хумаш").isDefined)


  @Test
  def findTanach = assertTrue(Works.workByName("Tanach").isDefined)


  @Test
  def bookSelector = {
    val bookOption = Works.getWorkByName("Tanach").selectorByName("book")
    assertTrue(bookOption.isDefined)
    val book = bookOption.get
    assertEquals(Set("chapter", "week"), book.selectors.map(_.defaultName).toSet)
    assertFalse(book.isNumbered)
  }


  @Test
  def chapterSelector = {
    val chapterOption = Works.getWorkByName("Tanach").selectorByName("book").get.selectorByName("chapter")
    assertTrue(chapterOption.isDefined)
    val chapter = chapterOption.get
    assertEquals(Set("verse"), chapter.selectors.map(_.defaultName).toSet)
    assertTrue(chapter.isNumbered)
  }


  @Test
  def defaultName = assertEquals("Tanach", Works.getWorkByName("Хумаш").defaultName)


  @Test
  def directory = assertEquals("Tanach", Works.getWorkByName("Хумаш").directory.getName)


  @Test
  def findTorontoEdition = assertTrue(Works.getWorkByName("Tanach").editionByName("Toronto").isDefined)


  @Test
  def genesisWeekStructure {
    Selection("Chumash").steps("book/Genesis/week").asDiv.div("Genesis")
  }


//  @Test
  def chumashFormats = {
    val formats: Seq[Seq[Selector]] = Works.getWorkByName("Хумаш").formats
    formats.map(_.map(_.defaultName).mkString("/")).foreach(println)
  }


//  @Test
  def genesisFormats = {
    val selection = Selection("Хумаш").div("book", "Genesis")
    val formats: Seq[Seq[Selector]] = selection.structures.formats
    formats.map(_.map(_.defaultName).mkString("/")).foreach(println)
  }


  @Test
  def jerusalemEditionStorage = {
    val storage = Works.getWorkByName("Tanach").getEditionByName("Jerusalem").storage
    assertTrue(storage.isDirectory)
    assertEquals("book", storage.asDirectory.structure.selector.defaultName)
    assertEquals(5, storage.asDirectory.files.size)
  }


//  @Test
  def genesisContent {
    val xml = Selection("Tanach", "Jerusalem").steps("book/Genesis").asStructure.xmlContent
  }


//  @Test
//  def firstPosuk {
//    Selection("Tanach", "Jerusalem").steps("book/Genesis/chapter/1")
//    Selection("Tanach", "Jerusalem").steps("book/Genesis/chapter/1/verse/1")
//  }
}
