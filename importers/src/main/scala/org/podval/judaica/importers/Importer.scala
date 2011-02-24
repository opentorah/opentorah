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

import scala.xml.{Node, PrettyPrinter}

import java.io.{File, FileWriter, PrintWriter}


abstract class Importer(inputDirectoryPath: String, outputDirectoryPath: String) {

    private val inputDirectory = new File(inputDirectoryPath)


    private val outputDirectory = new File(outputDirectoryPath)


    final def importBook(inputName: String, outputName: String) {
        val inFile = new File(inputDirectory, inputName+"." + getInputExtension())
        val xml = parseBook(inFile)
        val result = processBook(xml, outputName)
        val html = wrapInHtml(result);
        print(html, new File(outputDirectory, outputName+".html")) // CSS only works with ".html", not ".xml"!
    }


    private def wrapInHtml(what: Node) = {
        <html>
            <head>
              <link rel="stylesheet" type="text/css" href={"/css/" + getStylesheet()}/>
            </head>
            <body>
              {what}
            </body>
        </html>
    }


    private def print(xml: Node, outFile: File) {
        val out = new PrintWriter(new FileWriter(outFile))
        val pretty = new PrettyPrinter(100, 4).format(xml);
        out.println("<!DOCTYPE html>\n" + pretty)
        out.close()
    }


    def getInputExtension() : String


    def getStylesheet(): String


    def parseBook(file: File): Node


    def processBook(xml: Node, outputName: String): Node = {
        xml
    }
}
