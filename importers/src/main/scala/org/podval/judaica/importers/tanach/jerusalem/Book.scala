/*
 * Copyright 2010 dub.
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
 * under the License.
 */

package org.podval.judaica.importers.tanach.jerusalem

import org.podval.judaica.importers.AlefBeth

import scala.xml.{Elem, PrettyPrinter, XML, Node}

import scala.io.Source

import java.io.{File, FileWriter, PrintWriter}


class Book(inputFile: File, metadataFile: File, outputFile: File) {

    def run() {
        val lines = Source.fromFile(inputFile, "UTF-16BE").getLines().map(_.trim)
        val bookName = lines.next()
        val metadata = XML.loadFile(metadataFile) \\ "meta" \\ "_"

        val xml =
            <div type="book" n={bookName}>{
                lines.filter(!_.isEmpty).filter(!isChapter(_)).zipWithIndex.map({
                    case (line, chapterNumberFrom0) =>
                        val chapterNumber = chapterNumberFrom0+1
                        val chapterMetadata = metadata.filter(e => (e \ "@chapter").text == chapterNumber.toString)
                        <div type="chapter" n={chapterNumber.toString}>{
                            dropStuckChapter(line.split(":").map(_.trim)).zipWithIndex.map({
                                case (verse, verseNumberFrom0) =>
                                    val verseNumber = verseNumberFrom0+1
                                    val verseMetadata = chapterMetadata.filter(e => (e \ "@verse").text == verseNumber.toString)
                                    new Verse(verse, verseNumber, processMetadata(verseMetadata)).parse()
                            })
                        }
                        </div>
                    })
                }
            </div>
        
        print(xml);
    }


    private def dropStuckChapter(what: Seq[String]) =
        if (isChapter(what.last)) what.dropRight(1) else what


    private val PEREK = AlefBeth.PEI + AlefBeth.RESH + AlefBeth.QOF


    private def isChapter(line: String): Boolean = {
        line.startsWith(PEREK);
    }


    private def processMetadata(what: Seq[Node]): Seq[Node] = {
        // TODO!
        what
    }


    private def print(xml: Elem) {
        val pretty = new PrettyPrinter(100, 4).format(xml);
        val out = new PrintWriter(new FileWriter(outputFile))
        out.println(pretty)
        out.close()
    }
}
