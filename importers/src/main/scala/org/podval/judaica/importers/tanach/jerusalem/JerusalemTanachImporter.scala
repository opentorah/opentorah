package org.podval.judaica.importers.tanach.jerusalem

import scala.collection.mutable.ArrayBuffer
import scala.xml.Node
import scala.io.Source
import java.io.File

import org.opentorah.store.HebrewLanguage

object JerusalemTanachImporter {

  val SEFER: String = HebrewLanguage.SAMEH + HebrewLanguage.PEI + HebrewLanguage.RESH
  val PEREK: String = HebrewLanguage.PEI + HebrewLanguage.RESH + HebrewLanguage.QOF
  val PEI3: String = HebrewLanguage.PEI + " " + HebrewLanguage.PEI + " " + HebrewLanguage.PEI
  val SAMEH3: String = HebrewLanguage.SAMEH + " " + HebrewLanguage.SAMEH + " " + HebrewLanguage.SAMEH

  val HAZI: String =
    HebrewLanguage.HET +
    HebrewLanguage.TSADI +
    HebrewLanguage.YOD +
    " " +
    HebrewLanguage.HE +
    HebrewLanguage.SAMEH +
    HebrewLanguage.PEI +
    HebrewLanguage.RESH +
    " " +
    HebrewLanguage.BET +
    HebrewLanguage.PEI +
    HebrewLanguage.SAMEH +
    HebrewLanguage.VAV +
    HebrewLanguage.QOF +
    HebrewLanguage.YOD +
    HebrewLanguage.MEM_SOFIT


  val HAZAK: String = HebrewLanguage.HET + HebrewLanguage.ZAYIN + HebrewLanguage.QOF
}



final class JerusalemTanachImporter {
  protected override def parseBook(inputFile: File, bookName: String): DivContent = {
    def dropStuckChapter(what: Seq[String]) = if (isChapter(what.last)) what.dropRight(1) else what
    //dropStuckChapter(chapter.split(":").toIndexedSeq.map(_.trim))
    def isChapter(line: String): Boolean = line.startsWith(JerusalemTanachImporter.PEREK)

    val lines = Source.fromFile(inputFile, "UTF-16BE").getLines().map(_.trim)
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
      result += DivContent(
        "verse",
        Some(number.toString),
        Node.NoAttributes,
        None,
        processWords(line) :+ SpanContent("sofpasuk", HebrewLanguage.SOF_PASUQ)
      )
    } else {
      println(s"*** skipping empty line; verse number $number ***")
    }

    result.toIndexedSeq
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
    if (line.consume(HebrewLanguage.PEI)            ) Some(paragraph(true , false, HebrewLanguage.PEI)) else
    if (line.consume(JerusalemTanachImporter.SAMEH3)) Some(paragraph(false, true , JerusalemTanachImporter.SAMEH3)) else
    if (line.consume(HebrewLanguage.SAMEH)          ) Some(paragraph(false, false, HebrewLanguage.SAMEH)) else
      None
  }


  private def processWord(line: Line): Seq[Content] = {
    val spaceIndex = line.indexOf(" ")
    val makafIndex = line.indexOf(HebrewLanguage.MAQAF)
    def isFirst(one: Int, two: Int): Boolean = (one != -1) && ((two == -1) || (two > one))
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

    val alternate = line.consumeBracketed()

    val withAlternate =
      if (alternate.isEmpty) wordContent
      else AppContent(Map(
        // TODO strip the vowels off?
        "write" -> Seq(wordContent),
        "read" -> Seq(word("[" + alternate.get + "]", Node.NoAttributes))
      ))

    if (!isPasek) Seq(withAlternate) else Seq(withAlternate, SpanContent("pasek", HebrewLanguage.PASEQ))
  }
}
