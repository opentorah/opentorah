/*
 *  Copyright 2011 Leonid Dubinsky <dub@podval.org>.
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
package tanach

import org.podval.judaica.common.Xml.{loadResource, getAttribute}

import scala.xml.Node


abstract class TanachImporter(inputDirectory: String, outputDirectory: String) extends Importer(inputDirectory, outputDirectory) {

    final def run {
        output2inputName foreach { case (inputName, outputName) => importBook(inputName, outputName) }
    }


    protected def output2inputName: Map[String, String]
  

    protected final override def getStylesheet = "tanach.css"


    protected final override def processBook(xml: Node, outputName: String): Node = {
        val breaks =
            loadResource(classOf[TanachImporter], outputName, "meta").child
            .groupBy(getAttribute("chapter"))
            .mapValues(_.groupBy(getAttribute("verse")))

        val result = addBreaks(breaks, xml)

        result
    }


    private def addBreaks(breaks: Map[String, Map[String, Seq[Node]]], xml: Node): Node = {
        // TODO intersperse breaks with the contents
        xml
    }
}
