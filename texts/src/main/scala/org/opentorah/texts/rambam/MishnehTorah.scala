package org.opentorah.texts.rambam

import org.opentorah.util.Collections
import org.podval.metadata.{Name, Names}
import org.podval.store.{By, NumberedStore, NumberedStores, Store, Stores}
import org.podval.xml.{Xml, XmlCodec, XmlParser}
import Xml.given
import zio.blocks.schema.Schema

object MishnehTorah extends Stores[?]:
  override lazy val names: Names = Names(work.names)

  final class Book(
    val number: Int,
    override val names: Names,
    val parts: Seq[Part]
  ) extends Stores[?]:
    override lazy val stores: Seq[By[?]] = Seq(By("part", parts))

  sealed abstract class Part(
    val number: Int,
    val numChapters: Int,
    override val names: Names
  ) extends Stores[?]:
    def chapters: Seq[Chapter]

  final class PartWithNumberedChapters(
    number: Int,
    numChapters: Int,
    names: Names
  ) extends Part(number, numChapters, names):
    override def chapters: Seq[NumberedChapter] = byChapter.stores
    override lazy val stores: Seq[By[?]] = Seq(byChapter)
    private lazy val byChapter: By.Numbered[NumberedChapter] = By.Numbered("chapter", 1, numChapters)(NumberedChapter(_, _))

  final class PartWithNamedChapters(
    number: Int,
    numChapters: Int,
    names: Names,
    override val chapters: Seq[NamedChapter]
  ) extends Part(number, numChapters, names):
    require(numChapters == chapters.length)
    override lazy val stores: Seq[By[?]] = Seq(By("chapter", chapters))

  sealed abstract class Chapter extends Store

  final class NumberedChapter(
    override val number: Int,
    override val oneOf: NumberedStores[NumberedChapter]
  ) extends Chapter, NumberedStore

  final class NamedChapter(override val names: Names) extends Chapter

  private final case class BookDto(
    n: Int,
    names: Seq[Name] = Seq.empty,
    parts: Seq[PartDto] = Seq.empty
  ) derives CanEqual

  private object BookDto:
    given schema: Schema[BookDto] = Schema.derived
    val codec: XmlCodec[BookDto] = XmlCodec.derived(element = "book", PartDto.codec)
    def toBook(dto: BookDto): Book =
      val parts: Seq[Part] = dto.parts.map(PartDto.toPart)
      Collections.requireConsecutive(parts, _.number, "part")
      Book(dto.n, Names(dto.names), parts)

  private final case class PartDto(
    n: Int,
    chapters: Int,
    names: Seq[Name] = Seq.empty,
    chapterElems: Seq[ChapterDto] = Seq.empty
  ) derives CanEqual

  private object PartDto:
    given schema: Schema[PartDto] = Schema.derived
    val codec: XmlCodec[PartDto] = XmlCodec.derived(element = "part", ChapterDto.codec)
    def toPart(dto: PartDto): Part =
      val names: Names = Names(dto.names)
      if dto.chapterElems.isEmpty then PartWithNumberedChapters(dto.n, dto.chapters, names)
      else
        val chapters: Seq[NamedChapter] = dto.chapterElems.map(c => NamedChapter(Names(c.names)))
        PartWithNamedChapters(dto.n, dto.chapters, names, chapters)

  private final case class ChapterDto(
    names: Seq[Name] = Seq.empty
  ) derives CanEqual

  private object ChapterDto:
    given schema: Schema[ChapterDto] = Schema.derived
    val codec: XmlCodec[ChapterDto] = XmlCodec.derived(element = "chapter")

  private final case class WorkDto(
    names: Seq[Name] = Seq.empty,
    books: Seq[BookDto] = Seq.empty
  ) derives CanEqual

  private object WorkDto:
    given schema: Schema[WorkDto] = Schema.derived
    val codec: XmlCodec[WorkDto] = XmlCodec.derived(BookDto.codec)

  private lazy val work: WorkDto =
    XmlParser.parseResource[Xml.Element](getClass, "MishnehTorah.xml")
      .flatMap(WorkDto.codec.decode)
      .fold(error => throw error, identity)

  lazy val books: Seq[Book] =
    val result: Seq[Book] = work.books.map(BookDto.toBook)
    Collections.requireConsecutive(result, _.number, "book", from = 0, count = Some(15))
    result

  override lazy val stores: Seq[Store] = Seq(By("book", books))
