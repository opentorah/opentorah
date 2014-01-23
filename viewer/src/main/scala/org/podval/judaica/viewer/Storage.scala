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
import org.podval.judaica.xml.XmlFile

import scala.xml.Elem

import java.io.File


sealed trait Storage {
  def isDirectory: Boolean
  def isFile: Boolean
  def asDirectory: DirectoryStorage
  def asFile: FileStorage

  def content(path: Selection.Path, format: Selector.Format): Elem
}


// TODO segregate parsing

final class DirectoryStorage private(structures: Structures, xml: Elem, directory: File) extends Storage {
  override def isDirectory: Boolean = true
  override def isFile: Boolean = false
  override def asDirectory: DirectoryStorage = this
  override def asFile: FileStorage = throw new ClassCastException

  // TODO allow overrides ? val overrides = Xml.elems(structure) ... Xml.check(override, "file")

  val selector: Selector = structures.getSelectorByName(xml.getAttribute("structure"))
  val structure = structures.getStructure(selector)
  val fileFormat = structure.selector.parseFormat(xml.attributeOption("format"))

  val storage: Map[Div, Storage] = structure.divs.map { div =>  (div, forDiv(div)) }.toMap


  private[this] def forDiv(div: Div): Storage = {
    val fileCandidate = new File(directory, div.id + ".xml")
    val directoryCandidate = new File(directory, div.id)
    if (fileCandidate.exists && directoryCandidate.exists) throw new ViewerException(s"Only one of the files $fileCandidate or $directoryCandidate must exist")

    if (fileCandidate.exists) {
      if (!fileCandidate.isFile) throw new ViewerException(s"$fileCandidate must be a file")
      new FileStorage(fileCandidate)
    } else if (directoryCandidate.exists) {
      if (!directoryCandidate.isDirectory) throw new ViewerException(s"$fileCandidate must be a directory")
      ParseException.withMetadataFile(Exists(new File(directoryCandidate, "index.xml")))(DirectoryStorage(div, _, directoryCandidate))
    } else {
      throw new ViewerException(s"One of the files $fileCandidate or $directoryCandidate must exist")
    }
  }


  override def content(path: Selection.Path, format: Selector.Format): Elem = {
    val rawContent: Seq[Elem] = if (path.isEmpty) {
      structure.divs.map(div => storage(div).content(path, format))
    } else {
      val leadingDiv = path.head
      val leadingStructure = leadingDiv.structure
      if (leadingStructure != structure) throw new ViewerException(s"Restructuring from $structure to $leadingStructure isn't yet supported")
      Seq(storage(leadingDiv).content(path.tail, format))
    }

    <div type={structure.defaultName}>{rawContent}</div>
  }
}


object DirectoryStorage {

  def apply(structures: Structures, xml: Elem, directory: File): DirectoryStorage =
    new DirectoryStorage(structures, xml.oneChild("storage"), directory)
}



final class FileStorage(val file: File) extends Storage {
  override def isDirectory: Boolean = false
  override def isFile: Boolean = true
  override def asDirectory: DirectoryStorage = throw new ClassCastException
  override def asFile: FileStorage = this

  override def content(path: Selection.Path, format: Selector.Format): Elem = {
    content(XmlFile.load(file), path, format)
  }


  private[this] def content(xml: Elem, path: Selection.Path, format: Selector.Format): Elem = {
    // TODO process format - and compare with the file format :)
    if (path.isEmpty) xml else {
      val div = path.head
      // TODO at the last step, elements preceding the selected div which are not of the same selector should be retrieved also...
      val selection = select(xml.elems, div)
      val result = content(selection, path.tail, format)
      xml.copy(child = result)
    }
  }


  private[this] def select(xml: Seq[Elem], div: Div): Elem = {
    xml.find(e => (e.label == "div") && (e.getAttribute("type") == div.structure.defaultName) && (e.getAttribute("n") == div.id)).get
  }
}
