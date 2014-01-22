/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.{AlefBeth, Word, Div, Paragraph, App}
import org.podval.judaica.viewer.Works

import scala.xml.Elem

import scala.io.Source

import java.io.File


import scala.collection.mutable.ArrayBuffer


object JerusalemTanachImporter {

  def main(args: Array[String]) {
    val importer = new JerusalemTanachImporter("/home/dub/Code/judaica/imports/Tanach/jerusalem")

    importer.run
  }


  val PEREK = AlefBeth.PEI + AlefBeth.RESH + AlefBeth.QOF


  val PEI3 = AlefBeth.PEI + " " + AlefBeth.PEI + " " + AlefBeth.PEI


  val SAMEH3 = AlefBeth.SAMEH + " " + AlefBeth.SAMEH + " " + AlefBeth.SAMEH


  val HAZI =
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


  val HAZAK = AlefBeth.HET + AlefBeth.ZAYIN + AlefBeth.QOF
}



final class JerusalemTanachImporter(inputDirectory: String)
  extends TanachImporter(inputDirectory, Works.getWorkByName("Tanach").getEditionByName("Jerusalem"))
{
  protected override def getInputExtension: String = "txt"


  protected override def output2inputName: Map[String, String] = Map(
    "bereishis" -> "Genesis",
    "shemos" -> "Exodus",
    "vayikro" -> "Leviticus",
    "bamidbor" -> "Numbers",
    "devorim" -> "Deuteronomy"
  )


  protected override def parseBook(inputFile: File): Elem = {
    def dropStuckChapter(what: Seq[String]) = if (isChapter(what.last)) what.dropRight(1) else what
    def isChapter(line: String): Boolean = line.startsWith(JerusalemTanachImporter.PEREK)

    val lines = Source.fromFile(inputFile, "UTF-16BE").getLines().map(_.trim)
    val bookName = lines.next.trim

    Div("book", bookName, lines.filterNot(_.isEmpty).filterNot(isChapter).toSeq.zipWithIndex.map {
      case (chapter, chapterNumberFrom0) =>
        Div("chapter", (chapterNumberFrom0+1).toString,
          dropStuckChapter(chapter.split(":").map(_.trim)).zipWithIndex.flatMap {
            case (verse, verseNumberFrom0) =>
              parseVerse(verse, verseNumberFrom0+1)
          }
        )
    })
  }


  private def parseVerse(verse: String, number: Int): Seq[Elem] = {
    val result = new ArrayBuffer[Elem]

    val line = new Line(verse)

    result ++= processParagraph(line)

    line.consume(JerusalemTanachImporter.HAZI)
    line.consume(JerusalemTanachImporter.HAZAK)

    line.consumeBracketed()

    // TODO: if the line *is* empty, we skip a posuk number?!
    if (!line.isEmpty) {
      line.consumeToSpace()
      result += Div("verse", number.toString, processWords(line))
    }

    result
  }


  private def processParagraph(line: Line): Option[Elem] = {
    if (line.consume(JerusalemTanachImporter.PEI3)  ) Some(Paragraph(true , true )) else
    if (line.consume(AlefBeth.PEI)                  ) Some(Paragraph(true , false)) else
    if (line.consume(JerusalemTanachImporter.SAMEH3)) Some(Paragraph(false, true )) else
    if (line.consume(AlefBeth.SAMEH)                ) Some(Paragraph(false, false)) else
      None
  }


  // No unfold() in Scala...
  private def processWords(line: Line): Seq[Elem] = {
    val result = new ArrayBuffer[Elem]

    while (!line.isEmpty) {
      result += processWord(line)
    }

    result
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

    val isPasek = line.consume(AlefBeth.PIPE)

    val wordElement = Word(word, isMakaf, isPasek)

    val alternate = line.consumeBracketed() // TODO strip the vowels off?

    if (alternate.isEmpty) wordElement else App(wordElement, alternate.get)
  }
}
