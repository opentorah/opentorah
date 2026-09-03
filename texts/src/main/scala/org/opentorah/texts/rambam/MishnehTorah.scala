package org.opentorah.texts.rambam

import org.opentorah.util.Collections
import org.podval.metadata.{Name, Named, Names}
import org.podval.store.Selector
import org.podval.xml.{XmlCodec, XmlParser}
import zio.blocks.schema.{Modifier, Schema}

// TODO parse the names of the book itself! (and probably do the same for Tanach?)
object MishnehTorah:

  final class Book(
    val number: Int,
    override val names: Names,
    val parts: Seq[Part]
  ) extends Named

  sealed abstract class Part(
    val number: Int,
    val numChapters: Int,
    override val names: Names
  ) extends Named:
    def chapters: Seq[Chapter]

  final class PartWithNumberedChapters(
    number: Int,
    numChapters: Int,
    names: Names
  ) extends Part(number, numChapters, names):
    override def chapters: Seq[NumberedChapter] = (1 to numChapters).map(NumberedChapter(_))

  final class PartWithNamedChapters(
    number: Int,
    numChapters: Int,
    names: Names,
    override val chapters: Seq[NamedChapter]
  ) extends Part(number, numChapters, names):
    require(numChapters == chapters.length)

  sealed abstract class Chapter extends Named

  final class NumberedChapter(number: Int) extends Chapter:
    override def names: Names = Selector.getForName("chapter").andNumber(number).names

  final class NamedChapter(override val names: Names) extends Chapter

  @Modifier.config(XmlCodec.Element, "book")
  private final case class BookDto(
    @Modifier.config(XmlCodec.Attribute, "") n: Int,
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "part") parts: Seq[PartDto] = Seq.empty
  ) derives CanEqual

  private object BookDto:
    given schema: Schema[BookDto] = Schema.derived
    val codec: XmlCodec[BookDto] = XmlCodec.derived
    def toBook(dto: BookDto): Book =
      val parts: Seq[Part] = dto.parts.map(PartDto.toPart)
      Collections.requireConsecutive(parts, _.number, "part")
      Book(dto.n, Names(dto.names.map(Name.fromData)), parts)

  private final case class PartDto(
    @Modifier.config(XmlCodec.Attribute, "") n: Int,
    @Modifier.config(XmlCodec.Attribute, "") chapters: Int,
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "chapter") chapterElems: Seq[ChapterDto] = Seq.empty
  ) derives CanEqual

  private object PartDto:
    def toPart(dto: PartDto): Part =
      val names: Names = Names(dto.names.map(Name.fromData))
      if dto.chapterElems.isEmpty then PartWithNumberedChapters(dto.n, dto.chapters, names)
      else
        val chapters: Seq[NamedChapter] = dto.chapterElems.map(c => NamedChapter(Names(c.names.map(Name.fromData))))
        PartWithNamedChapters(dto.n, dto.chapters, names, chapters)

  private final case class ChapterDto(
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty
  ) derives CanEqual

  lazy val books: Seq[Book] =
    val result: Seq[Book] = XmlParser.loadCatalog(this, BookDto.codec).map(BookDto.toBook)
    Collections.requireConsecutive(result, _.number, "book", from = 0, count = Some(15))
    result
