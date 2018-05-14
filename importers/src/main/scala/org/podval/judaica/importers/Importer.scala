/*
 *  Copyright 2011-2018 Leonid Dubinsky <dub@podval.org>.
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  under the License.
 */

package org.podval.judaica.importers

import org.podval.judaica.viewer.{DivContent, Works, Edition, Content, Xml}

import java.io.File


trait Importer {
  def importWork(inputDirectoryPath: String): Unit = {
    val inputDirectory = new File(inputDirectoryPath)
    val edition: Edition = Works.getWorkByName(workName).getEditionByName(editionName)
    for ((inputName, outputName) <- books) {
      val inputFile: File = new File(inputDirectory, inputName + "." + getInputExtension)
      val outputFile: File = new File(edition.directory, outputName + ".xml") //  .storage.storage(outputName).asFile.file
      val content: DivContent = parseBook(inputFile, outputName)
      val result: DivContent = processBook(content, edition, outputName)
      Xml.print(Content.toXmlNode(result), outputFile)
    }
  }

  protected def workName: String

  protected def editionName: String

  protected def books: Map[String, String]

  protected def getInputExtension: String

  protected def parseBook(file: File, outputName: String): DivContent

  protected def processBook(content: DivContent, edition: Edition, outputName: String): DivContent = content
}
