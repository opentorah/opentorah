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

package org.podval.judaica.importers.tanach

import scala.xml.{Node, PrettyPrinter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import java.io.{File, PrintWriter, FileWriter}


abstract class TanachImporter(inputDirectory: String, outputDirectory: String) {

    protected final def importBook(inputName: String, outputName: String) {
        val xml = parseBook(new File(inputDirectory, inputName+".txt"))
        val result = addBreaks(Breaks.get(outputName), xml)
        print(result, new File(outputDirectory, outputName+".xml"))
    }


    protected def parseBook(inputFile: File): Node;


    private def addBreaks(breaks: mutable.Map[String, mutable.Map[String, ListBuffer[Node]]], xml: Node): Node = {
        xml
    }


    private def print(xml: Node, outputFile: File) {
        val pretty = new PrettyPrinter(100, 4).format(xml);
        val out = new PrintWriter(new FileWriter(outputFile))
        out.println(pretty)
        out.close()
    }
}
