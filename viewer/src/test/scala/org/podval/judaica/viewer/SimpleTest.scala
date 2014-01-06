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
import org.junit.Assert.{assertTrue, assertEquals}


// TODO switch to ScalaTest!
class SimpleTest {

  @Test
  def findChumash = assertTrue(Works.byName("Хумаш").isDefined)


  @Test
  def findTanach = assertTrue(Works.byName("Tanach").isDefined)


  @Test
  def bookSelector = {
    val book = Works.byName("Tanach").get.selectors.byName("book")
    assertTrue(book.isDefined)
    assertEquals(Set("chapter", "week"), book.get.selectors.map(_.names.default.name))
  }


  @Test
  def defaultName = assertEquals("Tanach", Works.byName("Хумаш").get.names.default.name)


  @Test
  def directory = assertEquals("Tanach", Works.byName("Хумаш").get.directory.getName)


  @Test
  def findTorontoEdition = assertTrue(Works.byName("Tanach").get.editions.byName("Toronto").isDefined)


  @Test
  def jerusalemEditionStorage = {
    val storage = Works.byName("Tanach").get.editions.byName("Jerusalem").get.storage
    assertTrue(storage.isDirectory)
    assertEquals("book", storage.asDirectory.structure.selector.names.default.name)
    assertEquals(5, storage.asDirectory.files.length)
    val file0 = storage.asDirectory.files(0)
    assertTrue(file0.isFile)
    assertEquals("Genesis.xml", file0.asFile.file.getName)
  }
}
