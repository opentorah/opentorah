/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.Xml.Ops
import org.podval.judaica.viewer.Edition

import scala.xml.Elem

import java.io.File


// TODO use <head> for the non-numeric names of the divs?
// TODO processing is harder - but merging is easier?

abstract class Importer(inputDirectoryPath: String, val edition: Edition) {

    private val inputDirectory = new File(inputDirectoryPath)


    private val outputDirectory = edition.directory


    final def importBook(inputName: String, outputName: String) {
        val inFile = new File(inputDirectory, inputName + "." + getInputExtension)
        val xml = parseBook(inFile)
        val result = processBook(xml, outputName)
        val outFile = new File(outputDirectory, outputName + ".xml")
        result.print(outFile)
    }


    protected def getInputExtension: String


    protected def parseBook(file: File): Elem


    protected def processBook(xml: Elem, outputName: String): Elem = xml
}
