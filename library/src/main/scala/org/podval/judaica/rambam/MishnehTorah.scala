package org.podval.judaica.rambam

import org.digitaljudaica.metadata.{Attributes, Language, Metadata, Name, Names, WithNames, Xml}
import scala.xml.Elem

object MishnehTorah {

  final class Book(val number: Int, override val names: Names, val parts: Seq[Part]) extends WithNames

  sealed abstract class Part(
    val number: Int,
    val numChapters: Int,
    override val names: Names
  ) extends WithNames {
    private var book_ : Option[Book] = None
    private[MishnehTorah] final def setBook(value: Book): Unit = book_ = Some(value)
    final def book: Book = book_.get

    def chapters: Seq[Chapter]
  }

  final class PartWithNumberedChapters(
    number: Int,
    numChapters: Int,
    names: Names
  ) extends Part(number, numChapters, names) {
    override def chapters: Seq[NumberedChapter] = (1 to numChapters).map(new NumberedChapter(this, _))
  }

  final class PartWithNamedChapters(
    number: Int,
    numChapters: Int,
    names: Names,
    override val chapters: Seq[NamedChapter]
  ) extends Part(number, numChapters, names) {
    require(numChapters == chapters.length)
  }

  sealed abstract class Chapter extends WithNames {
    def part: Part
  }

  final class NumberedChapter(override val part: Part, number: Int) extends Chapter {
    override def names: Names = chapterNames.withNumber(number)
  }

  private val chapterNames: Names = new Names(Seq(
    Name("פרק", Language.Hebrew.toSpec),
    Name("глава", Language.Russian.toSpec),
    Name("chapter", Language.English.toSpec)
  ))

  final class NamedChapter(override val names: Names) extends Chapter {
    private var part_ : Option[PartWithNamedChapters] = None
    private[MishnehTorah] def setPart(value: PartWithNamedChapters): Unit = part_ = Some(value)
    override def part: PartWithNamedChapters = part_.get
  }

  val books: Seq[Book] = {
    val result: Seq[Book] = Metadata
      .loadMetadataElements(this, None, "metadata", "book")
      .map(parseBook)

    require(result.map(_.number) == (0 to 14))

    result
  }

  private def parseBook(element: Elem): Book = {
    val (attributes: Attributes, elements: Seq[Elem]) = Xml.open(element, "book")
    val number: Int = attributes.doGet("n").toInt
    val (names: Names, tail: Seq[Elem]) = Names.parse(elements)
    val parts: Seq[Part] = Xml.span(tail, "part").map(parsePart)
    require(parts.map(_.number) == (1 to parts.length), s"Wrong part numbers: ${parts.map(_.number)} != ${1 until parts.length}")
    val result = new Book(number, names, parts)
    parts.foreach(_.setBook(result))
    result
  }

  private def parsePart(element: Elem): Part = {
    val (attributes: Attributes, elements: Seq[Elem]) = Xml.open(element, "part")
    val number: Int = attributes.doGetInt("n")
    val numChapters: Int = attributes.doGetInt("chapters")
    val (names: Names, tail: Seq[Elem]) = Names.parse(elements)
    if (tail.isEmpty) new PartWithNumberedChapters(
      number,
      numChapters,
      names
    ) else {
      val chapters: Seq[NamedChapter] = tail.map(parseNamedChapter)
      val result = new PartWithNamedChapters(
        number,
        numChapters,
        names,
        chapters
      )
      chapters.foreach(_.setPart(result))
      result
    }
  }

  private def parseNamedChapter(element: Elem): NamedChapter = {
    val (attributes: Attributes, elements: Seq[Elem]) = Xml.open(element, "chapter")
    val (names: Names, tail: Seq[Elem]) = Names.parse(elements)
    require(tail.isEmpty)
    new NamedChapter(names)
  }
}
