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
    val bookOption = Works.workByName("Tanach").get.selectorByName("book")
    assertTrue(bookOption.isDefined)
    val book = bookOption.get
    assertEquals(Set("chapter", "week"), book.selectors.map(_.names.default.name).toSet)
    assertFalse(book.isNumbered)
  }


  @Test
  def chapterSelector = {
    val chapterOption = Works.workByName("Tanach").get.selectorByName("book").get.selectorByName("chapter")
    assertTrue(chapterOption.isDefined)
    val chapter = chapterOption.get
    assertEquals(Set("verse"), chapter.selectors.map(_.names.default.name).toSet)
    assertTrue(chapter.isNumbered)
  }


  @Test
  def defaultName = assertEquals("Tanach", Works.workByName("Хумаш").get.names.default.name)


  @Test
  def directory = assertEquals("Tanach", Works.workByName("Хумаш").get.directory.getName)


  @Test
  def findTorontoEdition = assertTrue(Works.workByName("Tanach").get.editionByName("Toronto").isDefined)


//  @Test
//  def deepStructureChumash = {
//    val formats: Seq[Seq[Selector]] = Works.workByName("Хумаш").get.deepStructures
//    formats.map(_.map(_.names.default.name).mkString("/")).foreach(println)
//  }


  @Test
  def jerusalemEditionStorage = {
    val storage = Works.workByName("Tanach").get.editionByName("Jerusalem").get.storage
    assertTrue(storage.isDirectory)
    assertEquals("book", storage.asDirectory.structure.selector.names.default.name)
    assertEquals(5, storage.asDirectory.files.length)
    val file0 = storage.asDirectory.files(0)
    assertTrue(file0.isFile)
    assertEquals("Genesis.xml", file0.asFile.file.getName)
  }
}
