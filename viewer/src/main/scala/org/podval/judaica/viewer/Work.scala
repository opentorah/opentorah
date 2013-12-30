/*
 * Copyright 2012-2013 Podval Group.
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
import org.podval.judaica.xml.{Xml, Load}


final class Work private(override val names: Names, val directory: File, defaultEditionName: Option[String]) extends Named {

  lazy val editions: Seq[Edition] = directory.listFiles().filter(_.getName.endsWith(".xml")).map(Edition(this, _))


  def getEditionByName(name: String): Option[Edition] = editions.find(_.names.has(name))


  def defaultEdition: Option[Edition] = defaultEditionName flatMap  (getEditionByName(_))


  override def toString: String = "Work (" + directory + ") " + names
}



object Work {

  def apply(file: File): Work = {
    val xml = Load.loadFile(file, "work")
    val directoryName = Xml.getAttributeOption("directory")(xml).getOrElse(file.getName.dropRight(".xml".length))  // TODO centralize ".xml" processing...
    val directory = new File(file.getParentFile, directoryName)

    if (!directory.isDirectory) {
      throw new IllegalArgumentException("Not a directory: " + directory)
    }

    new Work(Names(xml), directory, Xml.getAttributeOption("defaultEdition")(xml))
  }
}
