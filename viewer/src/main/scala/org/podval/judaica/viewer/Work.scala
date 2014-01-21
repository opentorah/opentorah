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

import ParseException.withMetadataFile

import scala.xml.Elem

import java.io.File


object Works {

  val directory = new File("/home/dub/Code/judaica/texts/")

  def works: Seq[Work] = works_.get
  private[this] val works_ = LazyLoad(DirectoryScanner(directory, new Work(_, _)))


  def workByName(name: String): Option[Work] = Names.find(works, name)
  def getWorkByName(name: String): Work = Names.doFind(works, name, "work")

  def stylesheet: File = new File(directory, "stylesheet.css")
}



final class Work(val directory: File, index: File) extends Named with Structures {

  override val names: Names = withMetadataFile(index)(Names(_))


  override def selectors: Seq[Selector] = selectors_.get


  private[this] val selectors_ = LazyLoad(withMetadataFile(index)(xml => Exists(Selector.parse(Set.empty, xml), "selectors")))


  override def structures: Map[Selector, Structure] = structures_.get


  private[this] val structures_ = LazyLoad(withMetadataFile(index){ xml =>
    val context = Selector.ParsingContext(
      isDominant = true,
      dominantParentSelection = Selection(this),
      parsingFile = index,
      knownSelectors = Set.empty)

    parseStructures(context, xml)
  })


  // TODO handle situation when only one edition is present - allow to not put it into a subdirectory?
  def editions: Seq[Edition] = editions_.get
  def editionByName(name: String): Option[Edition] = Names.find(editions, name)
  def getEditionByName(name: String): Edition = Names.doFind(editions, name, "edition")
  private[this] val editions_ = LazyLoad(DirectoryScanner(directory, new Edition(this, _, _)))


  def stylesheet: File = new File(directory, "stylesheet.css")


  override def toString: String = "Work (" + directory + ") " + names
}



final class Edition(val work: Work, directory: File, index: File) extends Named {

  override val names: Names = withMetadataFile(index)(Names(_))

  // TODO add language attribute

  def storage: Storage = storage_.get
  private[this] val storage_ = LazyLoad(withMetadataFile(index)(DirectoryStorage(work, _, directory)))


  def stylesheet: File = new File(directory, "stylesheet.css")


  def content(path: Seq[Div], format: Seq[Selector]): Elem = storage.content(path, format)
}
