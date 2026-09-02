package org.opentorah.texts.rambam

import org.opentorah.metadata.{Name, Named, Names}
import org.opentorah.store.Selector
import org.podval.xml.{XmlAst, XmlCodec, XmlDecode, XmlError, XmlParser}

// TODO parse the names of the book itself! (and probably do the same for Tanach?)
object MishnehTorah:

  final class Book(
    val number: Int,
    override val names: Names,
    val parts: Seq[Part]
  ) extends Named

  object Book:
    // TODO why so much stuff? org.podval.xml not powerful enough?
    val codec: XmlCodec[Book] = new XmlCodec[Book]:
      override def elementName: String = "book"
      override def isRecordLike: Boolean = true

      override def unsafeDecode[E: XmlAst](element: E): Book =
        val names: Names = namesOf(element)
        val parts: Seq[Part] = XmlDecode.childrenNamed(element, "part").map(Part.codec.unsafeDecode(_))
        XmlDecode.requireNoOther(element, Set("name", "part"))
        require(parts.map(_.number) == (1 to parts.length),
          s"Wrong part numbers: ${parts.map(_.number)} != ${1 to parts.length}")
        val result: Book = Book(
          number = XmlDecode.intAttr(element, "n"),
          names = names,
          parts = parts
        )
        parts.foreach(_.setBook(result))
        result

      override def encodeNamed[E: XmlAst](elName: String, value: Book): E =
        throw XmlError("Book is decode-only")

  sealed abstract class Part(
    val number: Int,
    val numChapters: Int,
    override val names: Names
  ) extends Named:
    private var book_ : Option[Book] = None
    private[MishnehTorah] final def setBook(value: Book): Unit = book_ = Some(value)
    final def book: Book = book_.get

    def chapters: Seq[Chapter]

  object Part:
    // TODO why so much stuff? org.podval.xml not powerful enough?
    val codec: XmlCodec[Part] = new XmlCodec[Part]:
      override def elementName: String = "part"
      override def isRecordLike: Boolean = true

      override def unsafeDecode[E: XmlAst](element: E): Part =
        val number: Int = XmlDecode.positiveInt(element, "n")
        val numChapters: Int = XmlDecode.positiveInt(element, "chapters")
        val names: Names =
          try namesOf(element)
          catch case e: XmlError =>
            val kids: String = element.getChildren.flatMap(_.asElement).map: el =>
              s"${el.getName} attrs=${el.getAttributes} text=[${el.getText}]"
            .mkString(" || ")
            throw XmlError(s"${e.getMessage}; part children: $kids")
        val chapterElements: Seq[E] = XmlDecode.childrenNamed(element, "chapter")
        XmlDecode.requireNoOther(element, Set("name", "chapter"))
        if chapterElements.isEmpty then PartWithNumberedChapters(number, numChapters, names) else
          val chapters: Seq[NamedChapter] = chapterElements.map(el => NamedChapter(namesOf(el)))
          val result: PartWithNamedChapters = PartWithNamedChapters(number, numChapters, names, chapters)
          chapters.foreach(_.setPart(result))
          result

      override def encodeNamed[E: XmlAst](elName: String, value: Part): E =
        throw XmlError("Part is decode-only")

  final class PartWithNumberedChapters(
    number: Int,
    numChapters: Int,
    names: Names
  ) extends Part(number, numChapters, names):
    override def chapters: Seq[NumberedChapter] = (1 to numChapters).map(NumberedChapter(this, _))

  final class PartWithNamedChapters(
    number: Int,
    numChapters: Int,
    names: Names,
    override val chapters: Seq[NamedChapter]
  ) extends Part(number, numChapters, names):
    require(numChapters == chapters.length)

  sealed abstract class Chapter extends Named:
    def part: Part

  final class NumberedChapter(override val part: Part, number: Int) extends Chapter:
    override def names: Names = Selector.getForName("chapter").andNumber(number).names

  final class NamedChapter(override val names: Names) extends Chapter:
    private var part_ : Option[PartWithNamedChapters] = None
    private[MishnehTorah] def setPart(value: PartWithNamedChapters): Unit = part_ = Some(value)
    override def part: PartWithNamedChapters = part_.get

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val books: Seq[Book] =
    val result: Seq[Book] = XmlParser.loadCatalog(this, Book.codec)
    require(result.map(_.number) == (0 to 14))
    result

  private def namesOf[E: XmlAst](element: E): Names =
    Names(XmlDecode.childrenNamed(element, "name").map(Name.codec.unsafeDecode(_)))
