package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.metadata.{HasName, Names}
import org.podval.store.{By, Stores}
import org.podval.xml.{Xml, XmlAst, XmlCodec, XmlError, XmlParser}

trait TanachBook extends HasName, Stores[?] derives CanEqual: // all deriveds are objects; using eq

  private[tanach] final def chapters: Chapters = TanachBook.chapters(this)

  override def stores: Seq[By[?]] = Seq(chapters.byChapter)

  private[tanach] def parse(names: Names, chapters: Chapters, dto: BookDto): TanachBook.Parsed

private[tanach] object TanachBook:
  def valuesSeq: Seq[TanachBook] = Tanach.Book.valuesSeq

  private val codec: XmlCodec[Parsed] = new XmlCodec[Parsed]:
    override def elementName: String = "book"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Parsed =
      val dto: BookDto = BookDto.codec.unsafeDecode(element)
      val names: Names = dto.bookNames
      val chapters: Chapters = dto.chapterLengths
      val book: TanachBook = HasName.findByNames(valuesSeq, names)
      book.parse(names, chapters, dto)

    override def encodeNamed[E: XmlAst](elName: String, value: Parsed): E =
      throw XmlError("Tanach book is decode-only")

  private def loadBook(name: String): Parsed =
    XmlParser.parseResource[Xml.Element](Tanach.getClass, s"$name.xml")
      .flatMap(codec.decode)
      .fold(error => throw error, identity)

  private lazy val book2parsed: Map[TanachBook, Parsed] =
    val fromFiles: Seq[Parsed] =
      Seq("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy", "Psalms").map(loadBook)
    val parsed: Seq[Parsed] =
      (fromFiles ++ XmlParser.loadCatalog(Tanach, codec))
        .sortBy(p => valuesSeq.indexOf(p.book))
    val result: Map[TanachBook, Parsed] = parsed.map(metadata => metadata.book -> metadata).toMap
    val unmatched: Set[TanachBook] = valuesSeq.toSet -- result.keySet
    require(unmatched.isEmpty, s"Unmatched keys: $unmatched")
    require(result.size == parsed.length, s"Duplicate books in Tanach.xml")
    result

  def names(book: TanachBook): Names = book2parsed(book).names

  private def chapters(book: TanachBook): Chapters = book2parsed(book).chapters

  private lazy val book2metadata: Map[TanachBook, Metadata] = Collections.mapValues(book2parsed)(_.resolve)

  def metadata(book: TanachBook): Metadata = book2metadata(book)

  abstract class Metadata(
    val book: TanachBook
  )

  abstract class Parsed(
    val book: TanachBook,
    val names: Names,
    val chapters: Chapters
  ):
    def resolve: Metadata
