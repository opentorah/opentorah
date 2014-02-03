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


trait Work extends NamedDiv with DominantDiv {

  final override val structure: Structure = WorksStructure


  val directory: File


  override def names: Names


  override def selectors: Seq[Selector]


  override def structures: Map[Selector, NonRootStructure]


  def editions: Seq[Edition]


  final def editionByName(name: String): Option[Edition] = Names.find(editions, name)


  final def getEditionByName(name: String): Edition = Names.doFind(editions, name, "edition")


  final def stylesheet: File = new File(directory, "stylesheet.css")


  final override def toString: String = "Work (" + directory + ") " + names
}



object WorkSelector extends NamedSelector {

  override val names: Names = new Names(Seq(new Name("work", "en", isTransliterated = false)))


  override val selectors: Seq[Selector] = Seq.empty
}



object WorksStructure extends NamedStructure {

  override val selector: NamedSelector = WorkSelector


  override def isRoot: Boolean = true


  override def asNonRoot: NonRootStructure = throw new UnsupportedOperationException


  override val divs: Seq[NamedDiv] = Seq.empty // Or maybe the work we are dealing with?


  protected val lengthOption: Option[Int] = None
}
