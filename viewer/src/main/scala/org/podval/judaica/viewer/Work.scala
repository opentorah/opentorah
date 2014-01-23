/*
 * Copyright 2012-2014 Leonid Dubinsky <dub@podval.org>.
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

import java.io.File


// TODO introduce dominant/non-dominant Structure, so that the Work is covered too...
trait Work extends Named with Structures {

  val directory: File


  override def names: Names


  override def selectors: Seq[Selector]


  override def structures: Map[Selector, Structure]


  def editions: Seq[Edition]


  final def editionByName(name: String): Option[Edition] = Names.find(editions, name)


  final def getEditionByName(name: String): Edition = Names.doFind(editions, name, "edition")


  final def stylesheet: File = new File(directory, "stylesheet.css")


  final override def toString: String = "Work (" + directory + ") " + names
}
