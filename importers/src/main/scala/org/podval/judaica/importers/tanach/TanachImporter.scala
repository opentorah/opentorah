/*
 *  Copyright 2011 dub.
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

import scala.xml.Node

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


abstract class TanachImporter(inputDirectory: String, outputDirectory: String) extends Importer(inputDirectory, outputDirectory) {

    def getInputExtension() : String = "txt"


    def getStylesheet(): String = "tanach.css"


    override def processBook(xml: Node, outputName: String): Node = {
        addBreaks(Breaks.get(outputName), xml)
    }


    private def addBreaks(breaks: mutable.Map[String, mutable.Map[String, ListBuffer[Node]]], xml: Node): Node = {
        xml
    }
}
