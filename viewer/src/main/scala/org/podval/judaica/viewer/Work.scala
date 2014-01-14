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

import org.podval.judaica.xml.XmlFile
import scala.xml.Elem
import java.io.File


object Works {

  val textsDirectory = new File("/home/dub/Code/judaica/texts/")


  def works: Seq[Work] = works_.get
  private[this] val works_ = LazyLoad(DirectoryScanner(textsDirectory, new Work(_, _)))


  def workByName(name: String): Option[Work] = Names.find(works, name)
  def getWorkByName(name: String): Work = Exists(workByName(name), name, "work")
}



final class Work(val directory: File, index: File) extends Named with Selectors with Structures {

  override val names: Names = Names(metadata)


  override def selectors: Seq[Selector] = selectors_.get
  override def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
  private[this] val selectors_ = LazyLoad(Exists(Selector.parseSelectors(Seq.empty, metadata), "selectors"))


  override def structures: Seq[Structure] = structures_.get
  override def structureByName(name: String): Option[Structure] = Names.find(structures, name)
  private[this] val structures_ = LazyLoad(Exists(Structure.parseStructures(index, selectors, metadata), "structures"))


  def editions: Seq[Edition] = editions_.get
  def editionByName(name: String): Option[Edition] = Names.find(editions, name)
  def getEditionByName(name: String): Edition = Exists(editions, name, "edition")
  private[this] val editions_ = LazyLoad(DirectoryScanner(directory, new Edition(this, _, _)))


  private[this] def metadata: Elem = XmlFile.loadMetadata(index)


  override def toString: String = "Work (" + directory + ") " + names
}



final class Edition(val work: Work, directory: File, index: File) extends Named {

  override val names: Names = Names(metadata)

  // TODO add language attribute

  def storage: Storage = storage_.get
  private[this] val storage_ = LazyLoad(new DirectoryStorage(work.structures, metadata, directory))


  private[this] def metadata: Elem = XmlFile.loadMetadata(index)
}
