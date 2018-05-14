/*
 *  Copyright 2014-2018 Leonid Dubinsky <dub@podval.org>.
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
package org.podval.judaica.parsers

import java.io.File

import ParseException.withMetadataFile
import org.podval.judaica.viewer.{DirectoryStorage, Edition, Names, Work}
import Xml.Ops

object EditionParser {

  private final class ParseableEdition(
    override val work: Work,
    override val names: Names,
    override val language: String,
    override val directory: File,
    index: File) extends Edition
  {
    def storage: DirectoryStorage = storage_.get


    private[this] val storage_ = LazyLoad(withMetadataFile(index)(StorageParser.parseDirectoryStorage(work, _, directory)))
  }



  def parseEdition(work: Work, directory: File, index: File): Edition = {
    withMetadataFile(index) { xml =>
      val names = NamesParser.names(xml)
      val language = xml.attributeOption("language").getOrElse("he")
      new ParseableEdition(work, names, language, directory, index)
    }
  }
}
