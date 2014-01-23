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


trait DirectoryStorage extends Storage {

  final override def isDirectory: Boolean = true
  final override def isFile: Boolean = false
  final override def asDirectory: DirectoryStorage = this
  final override def asFile: FileStorage = throw new ClassCastException


  val directory: File


  // TODO allow overrides ? val overrides = Xml.elems(structure) ... Xml.check(override, "file")

  val structure: Structure


  val fileFormat: Selector.Format


  val storage: Map[Div, Storage]


  final def storage(id: String): Storage = storage(structure.divById(id).get)


  final override def content(path: Selection.Path, format: Selector.Format): Elem = {
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



trait FileStorage extends Storage {

  final override def isDirectory: Boolean = false
  final override def isFile: Boolean = true
  final override def asDirectory: DirectoryStorage = throw new ClassCastException
  final override def asFile: FileStorage = this


  val file: File


  final override def content(path: Selection.Path, format: Selector.Format): Elem = {
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
