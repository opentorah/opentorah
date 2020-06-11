package org.opentorah.texts.rambam

import org.opentorah.metadata.{Language, Metadata, Name, Names, WithNames}
import org.opentorah.xml.{Attribute, Element, From, Parser}

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

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val books: Seq[Book] = {
    val result: Seq[Book] = Metadata.load(
      from = From.resource(this),
      elementParsable = new Element(elementName = "book",  parser = bookParser)
    )

    require(result.map(_.number) == (0 to 14))

    result
  }

  private def bookParser: Parser[Book] = for {
    number <- Attribute("n").int.required
    names <- Names.parser
    parts <- new Element[Part](elementName = "part", parser = partParser).all
    _ <- Parser.check(parts.map(_.number) == (1 to parts.length),
      s"Wrong part numbers: ${parts.map(_.number)} != ${1 until parts.length}")
  } yield {
    val result = new Book(number, names, parts)
    parts.foreach(_.setBook(result))
    result
  }

  private def partParser: Parser[Part] = for {
    number <- Attribute("n").positiveInt.required
    numChapters <- Attribute("chapters").positiveInt.required
    names <- Names.parser
    chapters <- chapterParsable.all
  } yield {
    if (chapters.isEmpty) new PartWithNumberedChapters(number, numChapters, names) else {
      val result = new PartWithNamedChapters(number, numChapters, names, chapters)
      chapters.foreach(_.setPart(result))
      result
    }
  }

  object chapterParsable extends Element[NamedChapter](
    elementName = "chapter",
    parser = for {
      names <- Names.parser
    } yield new NamedChapter(names)
  )
}
