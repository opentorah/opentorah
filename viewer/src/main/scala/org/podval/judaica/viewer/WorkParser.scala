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

import org.podval.judaica.viewer.ParseException.withMetadataFile

import java.io.File


object WorkParser {

  private final class ParseableWork(override val directory: File, index: File) extends Work {

    override val names: Names = withMetadataFile(index)(Names(_))


    override def selectors: Seq[Selector] = selectors_.get


    private[this] val selectors_ = LazyLoad(withMetadataFile(index)(xml => Exists(SelectorParser.parseSelectors(Set.empty, xml), "selectors")))


    override def structures: Map[Selector, Structure] = structures_.get


    private[this] val structures_ = LazyLoad(withMetadataFile(index){ xml =>
      val context = SelectorParser.ParsingContext(
        isDominant = true,
        dominantParentSelection = Selection(this),
        parsingFile = index,
        knownSelectors = Set.empty)

      DivParser.parseStructures(context, this, xml)
    })


    override def editions: Seq[Edition] = editions_.get


    private[this] val editions_ = LazyLoad(DirectoryScanner(directory, EditionParser.parseEdition(this, _, _)))
  }


  def parseWork(directory: File, index: File): Work = new ParseableWork(directory, index)
}
