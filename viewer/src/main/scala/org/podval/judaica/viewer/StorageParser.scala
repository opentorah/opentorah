/*
 *  Copyright 2014 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.Xml.Ops

import scala.xml.Elem

import java.io.File


object StorageParser {

  private final class ParseableDirectoryStorage(
    override val directory: File,
    override val structure: Structure,
    override val fileFormat: Selector.Format,
    override val storage: Map[Div, Storage]) extends DirectoryStorage



  private final class ParseableFileStorage(override val file: File) extends FileStorage



  def parseDirectoryStorage(structures: Structures, xml0: Elem, directory: File): DirectoryStorage = {
    val xml = xml0.oneChild("storage")

    val selector: Selector = structures.getSelectorByName(xml.getAttribute("structure"))
    val structure = structures.getStructure(selector)
    val fileFormat = structure.selector.parseFormat(xml.attributeOption("format"))
    val storage: Map[Div, Storage] = structure.divs.map { div =>  (div, forDiv(directory, div)) }.toMap

    new ParseableDirectoryStorage(
      directory,
      structure,
      fileFormat,
      storage)
  }


  private def forDiv(directory: File, div: Div): Storage = {
    val fileCandidate = new File(directory, div.id + ".xml")
    val directoryCandidate = new File(directory, div.id)
    if (fileCandidate.exists && directoryCandidate.exists) throw new ViewerException(s"Only one of the files $fileCandidate or $directoryCandidate must exist")

    if (fileCandidate.exists) {
      if (!fileCandidate.isFile) throw new ViewerException(s"$fileCandidate must be a file")
      new ParseableFileStorage(fileCandidate)
    } else if (directoryCandidate.exists) {
      if (!directoryCandidate.isDirectory) throw new ViewerException(s"$fileCandidate must be a directory")
      ParseException.withMetadataFile(Exists(new File(directoryCandidate, "index.xml")))(parseDirectoryStorage(div, _, directoryCandidate))
    } else {
      throw new ViewerException(s"One of the files $fileCandidate or $directoryCandidate must exist")
    }
  }
}
