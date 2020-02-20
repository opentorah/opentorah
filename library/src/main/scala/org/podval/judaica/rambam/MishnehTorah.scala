package org.podval.judaica.rambam

import cats.implicits._
import org.digitaljudaica.metadata.{Language, Metadata, Name, Names, WithNames}
import org.digitaljudaica.xml.{From, Parser, Xml}

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
    val result: Seq[Book] = Metadata.load(
      from = From.resource(this),
      elementName = "book",
      parser = bookParser
    )

    require(result.map(_.number) == (0 to 14))

    result
  }

  private def bookParser: Parser[Book] = for {
    number <- Xml.attribute.required.positiveInt("n")
    names <- Names.parser
    parts <- Xml.all("part", partParser)
    _ <- Parser.check(parts.map(_.number) == (1 to parts.length),
      s"Wrong part numbers: ${parts.map(_.number)} != ${1 until parts.length}")
  } yield {
    val result = new Book(number, names, parts)
    parts.foreach(_.setBook(result))
    result
  }

  private def partParser: Parser[Part] = for {
    number <- Xml.attribute.required.positiveInt("n")
    numChapters <- Xml.attribute.required.positiveInt("chapters")
    names <- Names.parser
    chapters <- Xml.all[NamedChapter]("chapter", for {
      names <- Names.parser
    } yield new NamedChapter(names))
  } yield {
    if (chapters.isEmpty) new PartWithNumberedChapters(number, numChapters, names) else {
      val result = new PartWithNamedChapters(number, numChapters, names, chapters)
      chapters.foreach(_.setPart(result))
      result
    }
  }
}
