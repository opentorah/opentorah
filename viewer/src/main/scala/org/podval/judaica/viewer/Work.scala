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


import org.podval.judaica.xml.Load
import scala.xml.Elem
import java.io.File


object Works {

  val textsDirectory = new File("/home/dub/Code/judaica/texts/")


  def works: Seq[Work] = works_.get
  private[this] val works_ : Soft[Seq[Work]] =
    Soft(DirectoryScanner.describedDirectories(textsDirectory).map(d => new Work(d.name, d.directory, d.index)))


  def workByName(name: String): Option[Work] = Names.find(works, name)
}



final class Work(name: String, val directory: File, index: File) extends Named {

  override val names: Names = Names(/*name,*/ metadata) // TODO eliminate addition of the name


  def selectors: Seq[Selector] = selectors_.get
  def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
  private[this] val selectors_ : Soft[Seq[Selector]] = Soft(Parser.parseSelectors(Seq.empty, metadata))


  def structures: Seq[Structure] = structures_.get
  def structureByName(name: String): Option[Structure] = Names.find(structures, name)
  private[this] val structures_ : Soft[Seq[Structure]] = Soft(Parser.parseStructures(selectors, metadata))


  def editions: Seq[Edition] = editions_.get
  def editionByName(name: String): Option[Edition] = Names.find(editions, name)
  private[this] val editions_ : Soft[Seq[Edition]] =
    Soft(DirectoryScanner.describedDirectories(directory).map(d => new Edition(this, d.name, d.directory, d.index)))


  private[this] def metadata: Elem = Load.loadMetadata(index)


  override def toString: String = "Work (" + directory + ") " + names
}



final class Edition(val work: Work, name: String, directory: File, index: File) extends Named {

  override val names: Names = Names(/*name,*/ metadata) // TODO eliminate addition of the name


  def storage: Storage = storage_.get
  private[this] val storage_ : Soft[Storage] = Soft(new DirectoryStorage(work.structures, metadata, directory))


  private[this] def metadata: Elem = Load.loadMetadata(index)
}
