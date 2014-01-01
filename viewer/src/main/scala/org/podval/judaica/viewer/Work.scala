/*
 * Copyright 2012-2013 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.{Xml, Load}

import java.io.File
import scala.xml.Elem


final class Work private(name: String, metadata: Elem, val directory: File) extends Named {

  override val names = Names(name, metadata)


  private[this] val defaultEditionName = Xml.getAttributeOption("defaultEdition")(metadata)


  lazy val editions = new DirectoryScanner(directory).describedDirectories.map(d =>
    Edition(this, d.name, d.metadata, d.directory))


  def getEditionByName(name: String): Option[Edition] = Names.byName(name, editions)


  def defaultEdition: Option[Edition] = defaultEditionName flatMap (getEditionByName(_))


  override def toString: String = "Work (" + directory + ") " + names
}



object Work {

  def apply(name: String, metadata: File, directory: File): Work = new Work(name, Load.loadFile(metadata, "work"), directory)
}
