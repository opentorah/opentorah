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

import org.podval.judaica.xml.AlefBeth
import org.podval.judaica.viewer.{Content, DivContent, TextContent, AppContent}
import Content.prependAttribute

import scala.collection.mutable.ArrayBuffer
import scala.xml.{MetaData, Node}
import scala.io.Source

import java.io.File



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
  extends TanachImporter(inputDirectory, "Tanach", "Jerusalem")
{
  protected override def getInputExtension: String = "txt"


  protected override def output2inputName: Map[String, String] = Map(
    "bereishis" -> "Genesis"/*,
    "shemos" -> "Exodus",
    "vayikro" -> "Leviticus",
    "bamidbor" -> "Numbers",
    "devorim" -> "Deuteronomy"*/
  )


  protected override def parseBook(inputFile: File): Content = {
    def dropStuckChapter(what: Seq[String]) = if (isChapter(what.last)) what.dropRight(1) else what
    def isChapter(line: String): Boolean = line.startsWith(JerusalemTanachImporter.PEREK)

    val lines = Source.fromFile(inputFile, "UTF-16BE").getLines().map(_.trim)
    val bookName = lines.next.trim

    DivContent("book", Some(bookName), Node.NoAttributes, None, lines.filterNot(_.isEmpty).filterNot(isChapter).toSeq.zipWithIndex.map {
      case (chapter, chapterNumberFrom0) =>
        DivContent("chapter", Some((chapterNumberFrom0+1).toString), Node.NoAttributes, None,
          dropStuckChapter(chapter.split(":").map(_.trim)).zipWithIndex.flatMap {
            case (verse, verseNumberFrom0) =>
              parseVerse(verse, verseNumberFrom0+1)
          }
        )
    })
  }


  private def parseVerse(verse: String, number: Int): Seq[Content] = {
    val result = new ArrayBuffer[Content]

    val line = new Line(verse)

    result ++= processParagraph(line)

    line.consume(JerusalemTanachImporter.HAZI)
    line.consume(JerusalemTanachImporter.HAZAK)

    line.consumeBracketed()

    // TODO: if the line *is* empty, we skip a posuk number?!
    if (!line.isEmpty) {
      line.consumeToSpace()
      result += DivContent("verse", Some(number.toString), Node.NoAttributes, None, processWords(line))
    }

    result += TextContent(AlefBeth.SOF_PASUQ)

    result
  }


  private def processParagraph(line: Line): Option[Content] = {
    def paragraph(open: Boolean, big: Boolean, head: String): DivContent =
      DivContent(
        "paragraph",
        None,
        prependAttribute("open", open, prependAttribute("big", big, Node.NoAttributes)),
        Some(head),
        Seq.empty
      )

    if (line.consume(JerusalemTanachImporter.PEI3)  ) Some(paragraph(true , true , JerusalemTanachImporter.PEI3)) else
    if (line.consume(AlefBeth.PEI)                  ) Some(paragraph(true , false, AlefBeth.PEI)) else
    if (line.consume(JerusalemTanachImporter.SAMEH3)) Some(paragraph(false, true , JerusalemTanachImporter.SAMEH3)) else
    if (line.consume(AlefBeth.SAMEH)                ) Some(paragraph(false, false, AlefBeth.SAMEH)) else
      None
  }


  // No unfold() in Scala...
  private def processWords(line: Line): Seq[Content] = {
    val result = new ArrayBuffer[Seq[Content]]

    while (!line.isEmpty) {
      result += processWord(line)
    }

    result.flatten
  }


  private def processWord(line: Line): Seq[Content] = {
    def word(text: String, attributes: MetaData): DivContent =
      DivContent("word", None, attributes, None, Seq(TextContent(text)))


    val spaceIndex = line.indexOf(" ")
    val makafIndex = line.indexOf(AlefBeth.MAQAF)

    def isFirst(one: Int, two: Int) = (one != -1) && ((two == -1) || (two > one))
    val isSpace = isFirst(spaceIndex, makafIndex)
    val isMakaf = isFirst(makafIndex, spaceIndex)

    val index = if (isSpace) spaceIndex else if (isMakaf) makafIndex+1 else line.size
    val text = line.consumeToIndex(index)

    if (isSpace) {
      line.consume(" ")
    }

    val isPasek = line.consume("|")

    // TODO "word" shouldn't be a "div" in TEI, should it?
    val wordContent = word(text, prependAttribute("makaf", isMakaf, Node.NoAttributes))

    val alternate = line.consumeBracketed() // TODO strip the vowels off?

    val withAlternate =
      if (alternate.isEmpty) wordContent
      else AppContent(Map(
        "read" -> Seq(wordContent),
        "write" -> Seq(word(alternate.get, Node.NoAttributes))
      ))

    if (!isPasek) Seq(withAlternate) else Seq(withAlternate, TextContent(AlefBeth.PASEQ))
  }
}
