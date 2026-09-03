package org.opentorah.texts.rambam

import org.podval.metadata.{Name, Named, Names}
import org.podval.store.Selector
import org.podval.xml.{XmlCodec, XmlParser}
import zio.blocks.schema.{Modifier, Schema}

object SeferHamitzvosLessons:

  final class Lesson(
    val number: Int,
    val parts: Seq[Part]
  )

  sealed trait Part extends Named derives CanEqual

  final case class NamedPart(override val names: Names) extends Part

  sealed abstract class Commandment(val number: Int) extends Part:
    final override def names: Names = selector.andNumber(number).names
    def selector: Selector

  final case class Positive(override val number: Int) extends Commandment(number):
    override def selector: Selector = Selector.getForName("positive")

  final case class Negative(override val number: Int) extends Commandment(number):
    override def selector: Selector = Selector.getForName("negative")

  @Modifier.config(XmlCodec.Element, "lesson")
  private final case class LessonDto(
    @Modifier.config(XmlCodec.Attribute, "n") n: Int,
    parts: Seq[PartDto]
  ) derives CanEqual

  private object LessonDto:
    given schema: Schema[LessonDto] = Schema.derived
    val codec: XmlCodec[LessonDto] = schema.deriving(XmlCodec.deriver)
      .instance(zio.blocks.typeid.TypeId.of[PartDto], PartDto.codec)
      .derive
    def toLesson(dto: LessonDto): Lesson = Lesson(dto.n, dto.parts.map(PartDto.toPart))

  private sealed trait PartDto derives CanEqual

  @Modifier.config(XmlCodec.Element, "positive")
  private final case class PositiveDto(
    @Modifier.config(XmlCodec.Attribute, "n") n: Int
  ) extends PartDto derives CanEqual

  @Modifier.config(XmlCodec.Element, "negative")
  private final case class NegativeDto(
    @Modifier.config(XmlCodec.Attribute, "n") n: Int
  ) extends PartDto derives CanEqual

  @Modifier.config(XmlCodec.Element, "named")
  private final case class NamedDto(
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty
  ) extends PartDto derives CanEqual

  private object PartDto:
    given schema: Schema[PartDto] = Schema.derived
    val codec: XmlCodec[PartDto] = XmlCodec.derived
    def toPart(dto: PartDto): Part = dto match
      case PositiveDto(n) => Positive(n)
      case NegativeDto(n) => Negative(n)
      case NamedDto(names) => NamedPart(Names(names.map(Name.fromData)))

  lazy val lessons: Seq[Lesson] = XmlParser.loadCatalog(this, LessonDto.codec).map(LessonDto.toLesson)
