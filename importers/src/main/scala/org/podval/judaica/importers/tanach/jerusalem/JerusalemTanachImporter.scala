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

package org.podval.judaica.importers
package tanach
package jerusalem

import scala.xml.{Node, Elem, Text, NodeBuffer}

import scala.io.Source
import java.io.File


final class JerusalemTanachImporter(inputDirectory: String, outputDirectory: String) extends TanachImporter(inputDirectory, outputDirectory)  {

    def run() {
        importBook("bereishis", "Genesis");
//        importBook("shemos", "Exodus");
//        importBook("vayikro", "Leviticus");
//        importBook("bamidbor", "Numbers");
//        importBook("devorim", "Deuteronomy");
    }


    def parseBook(inputFile: File): Node = {
        val lines = Source.fromFile(inputFile, "UTF-16BE").getLines().map(_.trim)
        val bookName = lines.next()

        <div type="book" n={bookName}>{
            lines.filterNot(_.isEmpty).filterNot(isChapter).zipWithIndex.map({
                case (line, chapterNumberFrom0) =>
                    <div type="chapter" n={(chapterNumberFrom0+1).toString}>{
                        dropStuckChapter(line.split(":").map(_.trim)).zipWithIndex.map({
                            case (verse, verseNumberFrom0) =>
                                parseVerse(new Line(verse), verseNumberFrom0+1)
                        })
                    }
                    </div>
                })
            }
        </div>
    }


    private def dropStuckChapter(what: Seq[String]) =
        if (isChapter(what.last)) what.dropRight(1) else what


    private val PEREK = AlefBeth.PEI + AlefBeth.RESH + AlefBeth.QOF


    private def isChapter(line: String): Boolean = {
        line.startsWith(PEREK);
    }


    private val PEI3 = AlefBeth.PEI + " " + AlefBeth.PEI + " " + AlefBeth.PEI


    private val SAMEH3 = AlefBeth.SAMEH + " " + AlefBeth.SAMEH + " " + AlefBeth.SAMEH


    private val HAZI =
        AlefBeth.HET +
        AlefBeth.TSADI +
        AlefBeth.YOD +
        " " +
        AlefBeth.HE +
        AlefBeth.SAMEH +
        AlefBeth.PEI +
        AlefBeth.RESH +
        " " +
        AlefBeth.BET +
        AlefBeth.PEI +
        AlefBeth.SAMEH +
        AlefBeth.VAV +
        AlefBeth.QOF +
        AlefBeth.YOD +
        AlefBeth.MEM_SOFIT


    private val HAZAK = AlefBeth.HET + AlefBeth.ZAYIN + AlefBeth.QOF


    private def parseVerse(line: Line, number: Int): Seq[Node] = {
        val result = new NodeBuffer()

        val parsha = processParsha(line)
        if (parsha.isDefined) {
            result += parsha.get
        }

        line.consume(HAZI)
        line.consume(HAZAK)

        line.consumeBracketed()

        // TODO: if the line *is* empty, we skip a posuk number?!
        if (!line.isEmpty) {
            line.consumeToSpace()

            result +=
                <div type="verse" n={number.toString}>
                    {processWords(line)}
                </div>
        }

        result
    }


    private def processParsha(line: Line): Option[Node] = {
        var parsha: Option[String] = None
        var big = false

        if (line.consume(PEI3)) {
            parsha = Some("open")
            big = true
        } else
        if (line.consume(AlefBeth.PEI)) {
            parsha = Some("open")
        } else
        if (line.consume(SAMEH3)) {
            parsha = Some("closed")
            big = true
        } else
        if (line.consume(AlefBeth.SAMEH)) {
            parsha = Some("closed")
        }

        parsha.map((n: String) => <parsha type={n} big={booleanAttribute(big)}/>)
    }


    private def processWords(line: Line): Seq[Node] = {
        val result = new NodeBuffer()

        while (!line.isEmpty) {
            val wordElement = processWord(line)

            val alternate = line.consumeBracketed()

            val element =
                if (alternate == null) {
                    wordElement
                } else {
                    <app>
                        <rdg type="write">
                            {wordElement}
                         </rdg>
                         <rdg type="read">
                             <word>{alternate}</word>
                         </rdg>
                    </app>
                }

            result += element
        }

        return result
    }


    private def processWord(line: Line): Elem = {
        val spaceIndex = line.indexOf(" ")
        val makafIndex = line.indexOf(AlefBeth.MAQAF)

        def isFirst(one: Int, two: Int) = (one != -1) && ((two == -1) || (two > one))
        val isSpace = isFirst(spaceIndex, makafIndex)
        val isMakaf = isFirst(makafIndex, spaceIndex)

        val index = if (isSpace) spaceIndex else if (isMakaf) makafIndex else line.size
        val word = line.consumeToIndex(index)

        if (isSpace) {
            line.consume(" ")
        } else
        if (isMakaf) {
          line.consume(AlefBeth.MAQAF)
        }

        val isPasek = line.consume(AlefBeth.PASEQ)

        <word makaf={booleanAttribute(isMakaf)} pasek={booleanAttribute(isPasek)}>{word}</word>
    }


    private def booleanAttribute(value: Boolean) = if (value) Some(Text("true")) else None
}
