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

import org.podval.judaica.xml.Xml

import scala.xml.Elem

import java.io.File


sealed trait Storage {

  def isDirectory: Boolean


  def isFile: Boolean


  def asDirectory: DirectoryStorage


  def asFile: FileStorage


  def isRoot: Boolean


  def asRoot: RootStorage


  def asNonRoot: NonRootStorage


  def root: RootStorage
}


trait NonRootStorage extends Storage {

  final override def isRoot: Boolean = false


  final override def asRoot: RootStorage = throw new ClassCastException


  final override def asNonRoot: NonRootStorage = this


  def parent: Storage


  final override def root: RootStorage = parent.root
}


trait RootStorage extends Storage {

  final override def isRoot: Boolean = true


  final override def asRoot: RootStorage = this


  final override def asNonRoot: NonRootStorage = throw new ClassCastException


  def edition: Edition


  final override def root: RootStorage = asRoot
}


abstract class DirectoryStorage(metadata: Elem, directory: File) extends Storage {

  override def isDirectory: Boolean = true


  override def isFile: Boolean = false


  override def asDirectory: DirectoryStorage = this


  override def asFile: FileStorage = throw new ClassCastException


  val structureXml = Xml.oneChild(metadata, "structure")


  val structureType : String = Xml.getAttribute(structureXml, "type")


  val structure: Structure = root.edition.work.structures.find(structureType).get

  // TODO allow overrides ? val overrides = Xml.elems(structure) ... Xml.check(override, "file")

  val files: Seq[Storage] = structure.divs.map { div =>
    // TODO handle ".xml" files, not just directories!
    val fileCandidate = new File(directory, div.names.default.name + ".xml")
    val directoryCandidate = new File(directory, div.names.default.name)
    require((fileCandidate.exists || directoryCandidate.exists) && !(fileCandidate.exists && directoryCandidate.exists))
    val file = if (fileCandidate.exists) fileCandidate else directoryCandidate

    if (file.isFile) {
      new FileStorage(this, file)
    } else {
      val metadata = DirectoryScanner.metadata(file.getName, directory, file).get
      new NonRootDirectoryStorage(this, metadata, file)
    }
  }
}


final class RootDirectoryStorage(override val edition: Edition, metadata: Elem, directory: File) extends
  DirectoryStorage(metadata, directory) with RootStorage


final class NonRootDirectoryStorage(override val parent: Storage, metadata: Elem, directory: File) extends
  DirectoryStorage(metadata, directory) with NonRootStorage


final class FileStorage(override val parent: Storage, val file: File) extends Storage with NonRootStorage {

  override def isDirectory: Boolean = false


  override def isFile: Boolean = true


  override def asDirectory: DirectoryStorage = throw new ClassCastException


  override def asFile: FileStorage = this
}
