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

import Xml.Ops

import scala.xml.Elem

import java.io.File


object StorageParser {

  private final class ParsedDirectoryStorage(
    override val directory: File,
    override val structure: NonRootStructure,
    override val storage: Map[Div, Storage]) extends DirectoryStorage



  private final class ParsedFileStorage(override val file: File) extends FileStorage



  def parseDirectoryStorage(div: Div, xml0: Elem, directory: File): DirectoryStorage = {
    val xml = xml0.oneChild("storage")

    val selector: Selector = div.getSelectorByName(xml.getAttribute("structure"))
    val structure: NonRootStructure = div.getStructure(selector)
    val storage: Map[Div, Storage] = structure.divs.map { div =>  (div, forDiv(directory, div)) }.toMap

    new ParsedDirectoryStorage(
      directory,
      structure,
      storage)
  }


  private def forDiv(directory: File, div: Div): Storage = {
    val fileCandidate = new File(directory, div.id + ".xml")
    val directoryCandidate = new File(directory, div.id)
    if (fileCandidate.exists && directoryCandidate.exists) throw new ViewerException(s"Only one of the files $fileCandidate or $directoryCandidate must exist")

    if (fileCandidate.exists) {
      if (!fileCandidate.isFile) throw new ViewerException(s"$fileCandidate must be a file")
      new ParsedFileStorage(fileCandidate)

    } else if (directoryCandidate.exists) {
      if (!directoryCandidate.isDirectory) throw new ViewerException(s"$fileCandidate must be a directory")
      ParseException.withMetadataFile(Exists(new File(directoryCandidate, "index.xml")))(parseDirectoryStorage(div, _, directoryCandidate))

    } else {
      throw new ViewerException(s"One of the files $fileCandidate or $directoryCandidate must exist")
    }
  }
}
