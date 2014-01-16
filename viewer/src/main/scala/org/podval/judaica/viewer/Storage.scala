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


sealed trait Storage {
  def isDirectory: Boolean
  def isFile: Boolean
  def asDirectory: DirectoryStorage
  def asFile: FileStorage

  def content(path: Seq[Div], format: Seq[Selector]): Elem
}



final class DirectoryStorage private(structures: Seq[Structure], xml: Elem, directory: File) extends Storage {
  override def isDirectory: Boolean = true
  override def isFile: Boolean = false
  override def asDirectory: DirectoryStorage = this
  override def asFile: FileStorage = throw new ClassCastException

  // TODO allow overrides ? val overrides = Xml.elems(structure) ... Xml.check(override, "file")

  val structure: Structure = Names.doFind(structures, xml.getAttribute("structure"), "structure")
  val fileFormat = structure.selector.parseFormat(xml.attributeOption("format"))

  val files: Map[Div, Storage] = structure.divs.map { div =>  (div, forDiv(div)) }.toMap


  private[this] def forDiv(div: Div): Storage = {
    val fileCandidate = new File(directory, div.id + ".xml")
    val directoryCandidate = new File(directory, div.id)
    if (fileCandidate.exists && directoryCandidate.exists) throw new ViewerException(s"Only one of the files $fileCandidate or $directoryCandidate must exist")

    if (fileCandidate.exists) {
      if (!fileCandidate.isFile) throw new ViewerException(s"$fileCandidate must be a file")
      new FileStorage(fileCandidate)
    } else if (directoryCandidate.exists) {
      if (!directoryCandidate.isDirectory) throw new ViewerException(s"$fileCandidate must be a directory")
      ParseException.withMetadataFile(Exists(new File(directoryCandidate, "index.xml")))(DirectoryStorage(div.structures, _, directoryCandidate))
    } else {
      throw new ViewerException(s"One of the files $fileCandidate or $directoryCandidate must exist")
    }
  }


  override def content(path: Seq[Div], format: Seq[Selector]): Elem = ???
}


object DirectoryStorage {

  def apply(structures: Seq[Structure], xml: Elem, directory: File): DirectoryStorage =
    new DirectoryStorage(structures, xml.oneChild("storage"), directory)
}

final class FileStorage(val file: File) extends Storage {
  override def isDirectory: Boolean = false
  override def isFile: Boolean = true
  override def asDirectory: DirectoryStorage = throw new ClassCastException
  override def asFile: FileStorage = this

  override def content(path: Seq[Div], format: Seq[Selector]): Elem = ???
}
