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

import scala.xml.{Elem, PrettyPrinter}

import scala.io.Source

import java.io.{File, FileWriter, PrintWriter}


class Book(inputFile: File, outputFile: File) {

    def run() {
        val lines = Source.fromFile(inputFile, "UTF-16BE").getLines().map(_.trim)
        val bookName = lines.next()

        val xml =
            <div type="book" n={bookName}>{
            var chapterNumber = 0
            for (line <- lines
                 if !line.isEmpty
                 if !isChapter(line)
            ) yield {
                chapterNumber += 1
                <div type="chapter" n={chapterNumber.toString}>{
                var verseNumber = 0;
                for (verse <- dropStuckChapter(line.split(":").map(_.trim()))) yield {
                    verseNumber += 1
                    new Verse(verse, verseNumber).parse()
                }
                }
                </div>
            }
            }
            </div>

//        insertBreaks(pipeline, outName);

        print(xml);
    }


    private def dropStuckChapter(what: Seq[String]) =
        if (isChapter(what.last)) what.dropRight(1) else what


    private def isChapter(line: String): Boolean = {
        line.startsWith("פרק"); // @todo alefbeth
    }


    private def print(xml: Elem) {
        val pretty = new PrettyPrinter(100, 4).format(xml);
        val out = new PrintWriter(new FileWriter(outputFile))
        out.println(pretty)
        out.close()
    }
}
