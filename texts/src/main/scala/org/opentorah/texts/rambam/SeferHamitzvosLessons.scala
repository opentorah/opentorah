package org.opentorah.texts.rambam

import org.podval.metadata.{Name, Names}
import org.podval.store.{By, NumberedStore, NumberedStores, Store, Stores}
import org.podval.xml.{XmlCodec, XmlParser}
import zio.blocks.schema.{Modifier, Schema}

object SeferHamitzvosLessons extends Stores[?]:
  override val names: Names = Names("Sefer Hamitzvos")

  final class Lesson(
    override val number: Int,
    val parts: Seq[Part],
    override val oneOf: NumberedStores[Lesson]
  ) extends NumberedStore, Stores[?]:
    override lazy val stores: Seq[Store] =
      val positives: Seq[Positive] = parts.collect { case part: Positive => part }
      val negatives: Seq[Negative] = parts.collect { case part: Negative => part }
      val named: Seq[NamedPart] = parts.collect { case part: NamedPart => part }
      Seq(
        Option.when(positives.nonEmpty)(By("positive", positives)),
        Option.when(negatives.nonEmpty)(By("negative", negatives))
      ).flatten ++ named

  sealed trait Part extends Store derives CanEqual

  final case class NamedPart(override val names: Names) extends Part

  sealed abstract class Commandment(val number: Int) extends Part:
    final override def names: Names = NumberedStores.namesForNumber(number)

  final case class Positive(override val number: Int) extends Commandment(number)

  final case class Negative(override val number: Int) extends Commandment(number)

  @Modifier.config(XmlCodec.Element, "lesson")
  private final case class LessonDto(
    n: Int,
    parts: Seq[PartDto]
  ) derives CanEqual

  private object LessonDto:
    given schema: Schema[LessonDto] = Schema.derived
    val codec: XmlCodec[LessonDto] = schema.deriving(XmlCodec.deriver)
      .instance(zio.blocks.typeid.TypeId.of[PartDto], PartDto.codec)
      .derive
    def toParts(dto: LessonDto): Seq[Part] = dto.parts.map(PartDto.toPart)

  private sealed trait PartDto derives CanEqual

  @Modifier.config(XmlCodec.Element, "positive")
  private final case class PositiveDto(
    n: Int
  ) extends PartDto derives CanEqual

  @Modifier.config(XmlCodec.Element, "negative")
  private final case class NegativeDto(
    n: Int
  ) extends PartDto derives CanEqual

  @Modifier.config(XmlCodec.Element, "named")
  private final case class NamedDto(
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name] = Seq.empty
  ) extends PartDto derives CanEqual

  private object PartDto:
    given schema: Schema[PartDto] = Schema.derived
    val codec: XmlCodec[PartDto] = XmlCodec.derived
    def toPart(dto: PartDto): Part = dto match
      case PositiveDto(n) => Positive(n)
      case NegativeDto(n) => Negative(n)
      case NamedDto(names) => NamedPart(Names(names))

  private lazy val lessonDtos: Seq[LessonDto] = XmlParser.loadCatalog(this, LessonDto.codec)

  private lazy val byLesson: By.Numbered[Lesson] = By.Numbered("lesson", 1, lessonDtos.length): (number, parent) =>
    val dto: LessonDto = lessonDtos(number - 1)
    require(dto.n == number, s"Lesson ${dto.n} at $number")
    Lesson(number, LessonDto.toParts(dto), parent)

  lazy val lessons: Seq[Lesson] = byLesson.stores

  override lazy val stores: Seq[Store] = Seq(
    byLesson,
    By("positive", lessons.flatMap(_.parts.collect { case part: Positive => part }).distinct.sortBy(_.number)),
    By("negative", lessons.flatMap(_.parts.collect { case part: Negative => part }).distinct.sortBy(_.number))
  )
