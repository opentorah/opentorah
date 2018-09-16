package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.XML

import scala.xml.Elem

object ChaptersParser {
  private final case class ChapterParsed(n: Int, length: Int)

  def parse(elements: Seq[Elem]): Chapters = {
    val chapters: Seq[ChapterParsed] = elements.map { element =>
      val attributes = XML.openEmpty(element, "chapter" )
      val result = ChapterParsed(
        n = attributes.doGetInt("n"),
        length = attributes.doGetInt("length")
      )
      attributes.close()
      result
    }

    require(chapters.map(_.n) == (1 to chapters.length), s"Wrong chapter numbers: $chapters")

    new Chapters(chapters.map(_.length).toArray)
  }
}
