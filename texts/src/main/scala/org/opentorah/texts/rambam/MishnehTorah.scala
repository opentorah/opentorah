package org.opentorah.texts.rambam

import org.opentorah.metadata.{Language, Name, Named, Names}
import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, Element, From, Parsable, Parser, Unparser}

object MishnehTorah {

  final class Book(val number: Int, override val names: Names, val parts: Seq[Part]) extends Named

  sealed abstract class Part(
    val number: Int,
    val numChapters: Int,
    override val names: Names
  ) extends Named {
    private var book_ : Option[Book] = None
    private[MishnehTorah] final def setBook(value: Book): Unit = book_ = Some(value)
    final def book: Book = book_.get

    def chapters: Seq[Chapter]
  }

  object Part extends Element[Part]("part") {
    private val nAttribute: Attribute.Required[Int] = new Attribute.PositiveIntAttribute("n").required
    private val chaptersAttribute: Attribute.Required[Int] = new Attribute.PositiveIntAttribute("chapters").required

    override def contentParsable: Parsable[Part] = new Parsable[Part] {
      override def parser: Parser[Part] = for {
        number <- nAttribute()
        numChapters <- chaptersAttribute()
        names <- Names.withoutDefaultNameParsable()
        chapters <- NamedChapter.seq()
      } yield {
        if (chapters.isEmpty) new PartWithNumberedChapters(number, numChapters, names) else {
          val result = new PartWithNamedChapters(number, numChapters, names, chapters)
          chapters.foreach(_.setPart(result))
          result
        }
      }

      override def unparser: Unparser[Part] = ???
    }
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

  sealed abstract class Chapter extends Named {
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

  object NamedChapter extends Element[NamedChapter]("chapter") {
    override def contentParsable: Parsable[NamedChapter] = new Parsable[NamedChapter] {
      override def parser: Parser[NamedChapter] = for {
        names <- Names.withoutDefaultNameParsable()
      } yield new NamedChapter(names)

      override def unparser: Unparser[NamedChapter] = Names.withoutDefaultNameParsable(_.names)
    }
  }

  object Book extends Element[Book]("book") {
    private val nAttribute: Attribute.Required[Int] = new Attribute.IntAttribute("n").required

    override def contentParsable: Parsable[Book] = new Parsable[Book] {
      override def parser: Parser[Book] = for {
        number <- nAttribute()
        names <- Names.withoutDefaultNameParsable()
        parts <- Part.seq()
        _ <- Effects.check(parts.map(_.number) == (1 to parts.length),
          s"Wrong part numbers: ${parts.map(_.number)} != ${1 until parts.length}")
      } yield {
        val result = new Book(number, names, parts)
        parts.foreach(_.setBook(result))
        result
      }

      override def unparser: Unparser[Book] = ???
    }
  }

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val books: Seq[Book] = {
    val result: Seq[Book] = Parser.unsafeRun(Named.load(From.resource(this), Book))
    require(result.map(_.number) == (0 to 14))
    result
  }
}
