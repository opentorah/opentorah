package org.opentorah.texts.rambam

import org.opentorah.metadata.{Named, Names}
import org.opentorah.store.Selector
import org.podval.xml.{XmlAst, XmlCodec, XmlDecode, XmlError, XmlParser}

object SeferHamitzvosLessons:

  final class Lesson(
    val number: Int,
    val parts: Seq[Part]
  )

  object Lesson:
    val codec: XmlCodec[Lesson] = new XmlCodec[Lesson]:
      override def elementName: String = "lesson"
      override def isRecordLike: Boolean = true

      override def unsafeDecode[E: XmlAst](element: E): Lesson =
        Lesson(
          number = XmlDecode.positiveInt(element, "n"),
          parts = element.getChildren.flatMap(_.asElement).map(Part.codec.unsafeDecode(_))
        )

      override def encodeNamed[E: XmlAst](elName: String, value: Lesson): E =
        throw XmlError("Lesson is decode-only")

  sealed trait Part extends Named derives CanEqual

  object Part:
    val codec: XmlCodec[Part] = new XmlCodec[Part]:
      override def elementName: String = "part"
      override def isRecordLike: Boolean = true
      override def caseNames: Seq[String] = Seq("positive", "negative", "named")

      override def unsafeDecode[E: XmlAst](element: E): Part =
        element.localName match
          case "positive" => Positive(XmlDecode.positiveInt(element, "n"))
          case "negative" => Negative(XmlDecode.positiveInt(element, "n"))
          case "named" => NamedPart(Names.codec.unsafeDecode(element))
          case other => throw XmlError(s"Unknown lesson part: $other")

      override def encodeNamed[E: XmlAst](elName: String, value: Part): E =
        throw XmlError("Part is decode-only")

  final case class NamedPart(override val names: Names) extends Part

  sealed abstract class Commandment(val number: Int) extends Part:
    final override def names: Names = selector.andNumber(number).names
    def selector: Selector

  final case class Positive(override val number: Int) extends Commandment(number):
    override def selector: Selector = Selector.getForName("positive")

  final case class Negative(override val number: Int) extends Commandment(number):
    override def selector: Selector = Selector.getForName("negative")

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val lessons: Seq[Lesson] = XmlParser.loadCatalog(this, Lesson.codec)
