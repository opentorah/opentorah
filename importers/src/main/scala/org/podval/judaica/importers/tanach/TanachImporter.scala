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

import scala.xml.{XML, Node, Utility, PrettyPrinter}


abstract class TanachImporter(inputDirectory: String, outputDirectory: String) extends Importer(inputDirectory, outputDirectory) {

    def getInputExtension() : String = "txt"

    protected final def importBook(inputName: String, outputName: String) {
        val xml = parseBook(new File(inputDirectory, inputName+".txt"))
        val breaks = getBreaks(outputName)
        val result = addBreaks(breaks, xml)
        print(result, new File(outputDirectory, outputName+".xml"))
    }


    def getStylesheet(): String = "tanach.css"


//    override def processBook(xml: Node, outputName: String): Node = {
//        addBreaks(Breaks.get(outputName), xml)
//    }


    protected def parseBook(inputFile: File): Node;


    private def getBreaks(name: String): Map[String, Map[String, Seq[Node]]] = {
        val xml = Utility.trimProper(XML.load(getClass.getResourceAsStream(name + ".xml")))(0).child

        xml.groupBy(getAttribute("@chapter")).mapValues(_.groupBy(getAttribute("@verse")))
    }


    private def getAttribute(name: String)(node: Node) = (node \ name).text


    private def addBreaks(breaks: Map[String, Map[String, Seq[Node]]], xml: Node): Node = {
        xml
    }
}
