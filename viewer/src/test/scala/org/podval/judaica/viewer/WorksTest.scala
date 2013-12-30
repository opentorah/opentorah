/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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


class WorksTest {

  @Test
  def findChumash = assertTrue(Works.instance.getByName("Хумаш").isDefined)


  @Test
  def findTanach = assertTrue(Works.instance.getByName("Tanach").isDefined)


  @Test
  def defaultName = assertEquals("Tanach", Works.instance.getByName("Хумаш").get.names.default.name)


  @Test
  def directory = assertEquals("Tanach", Works.instance.getByName("Хумаш").get.directory)
}
