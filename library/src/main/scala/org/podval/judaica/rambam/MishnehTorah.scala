package org.podval.judaica.rambam

import org.podval.judaica.metadata.{Attributes, Metadata, Names, WithNames, XML}

import scala.xml.Elem

object MishnehTorah {

  final class Book(val number: Int, override val names: Names, partsRaw: Seq[PartRaw]) extends WithNames {
    val parts: Seq[Part] = partsRaw.map(_.toPart(this))
  }

  private final class PartRaw(val number: Int, names: Names, numChapters: Int) {
    def toPart(book: Book): Part = new Part(number, names, numChapters, book)
  }

  final class Part(val number: Int, override val names: Names, val numChapters: Int, val book: Book) extends WithNames {
    def chapters: Seq[Chapter] = (1 to numChapters).map(number => new Chapter(number, this))
  }

  final class Chapter(val number: Int, val part: Part) {
    // TODO named chapters
    // TODO WithNames
  }

  val books: Seq[Book] = {
    val result: Seq[Book] = Metadata
      .loadMetadataElements(this, None, "metadata", "book")
      .map(parseBook)

    require(result.map(_.number) == (0 to 14))

    result
  }

  private def parseBook(element: Elem): Book = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "book")
    val number: Int = attributes.doGet("n").toInt
    val (names: Names, tail: Seq[Elem]) = Names.parse(elements)
    val parts: Seq[PartRaw] = XML.span(tail, "part").map(parsePart)
    require(parts.map(_.number) == (1 to parts.length), s"Wrong part numbers: ${parts.map(_.number)} != ${1 until parts.length}")
    new Book(number, names, parts)
  }

  private def parsePart(element: Elem): PartRaw = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "part")
    val number: Int = attributes.doGetInt("n")
    val numChapters: Int = attributes.doGetInt("chapters")
    val (names: Names, tail: Seq[Elem]) = Names.parse(elements)
    require(tail.isEmpty)
    new PartRaw(number, names, numChapters)
  }
}
