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

import org.podval.judaica.xml.Xml.XmlOps

import scala.xml.Elem

import java.io.File
import org.podval.judaica.xml.Load


// TODO factor the parsing out
sealed trait Storage {
  def isDirectory: Boolean
  def isFile: Boolean
  def asDirectory: DirectoryStorage
  def asFile: FileStorage
}


final class FileStorage(val file: File) extends Storage {
  override def isDirectory: Boolean = false
  override def isFile: Boolean = true
  override def asDirectory: DirectoryStorage = throw new ClassCastException
  override def asFile: FileStorage = this
}


final class DirectoryStorage(structures: Seq[Structure], metadata: Elem, directory: File) extends Storage {
  override def isDirectory: Boolean = true
  override def isFile: Boolean = false
  override def asDirectory: DirectoryStorage = this
  override def asFile: FileStorage = throw new ClassCastException

  val storageXml = metadata.oneChild("storage")
  val structureName : String = storageXml.getAttribute("structure")
  val structureOption: Option[Structure] = Names.find(structures, structureName)
  require(structureOption.isDefined, s"Structure $structureName not found")
  val structure = structureOption.get

  // TODO allow overrides ? val overrides = Xml.elems(structure) ... Xml.check(override, "file")

  val files: Seq[Storage] = {
    structure.divs map { div =>
      val name = div.id
      val fileCandidate = new File(directory, name + ".xml")
      val directoryCandidate = new File(directory, name)
      require((fileCandidate.exists || directoryCandidate.exists), s"One of the files $fileCandidate or $directoryCandidate must exist")
      require(!(fileCandidate.exists && directoryCandidate.exists), s"Only one of the files $fileCandidate or $directoryCandidate must exist")
      val file = if (fileCandidate.exists) fileCandidate else directoryCandidate

      if (file.isFile) {
        new FileStorage(file)
      } else {
        val metadataFile = DirectoryScanner.metadata(file.getName, directory, file).get
        val metadata = Load.loadMetadata(metadataFile)
        new DirectoryStorage(div.structures, metadata, file)
      }
    }
  }
}
