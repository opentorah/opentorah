package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, Language}
import org.opentorah.util.Collections
import org.opentorah.util.Effects
import org.podval.xml.{XmlAst, XmlCodec, XmlDecode, XmlParser}
import zio.blocks.schema.{Modifier, Schema}

// TODO de-case - and figure out why object Haftarah's creation becomes impossible if 'case' is removed here...
final case class Haftarah(override val spans: Seq[Haftarah.BookSpan]) extends Haftarah.Spans(spans) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: Haftarah => this.spans == that.spans
    case _ => false

object Haftarah extends WithBookSpans[Tanach.Prophets]:
  override type Many = Haftarah

  def toLanguageString(spans: Seq[BookSpan])(using spec: Language.Spec): String =
    Collections.group(spans, (span: BookSpan) => span.book)
      .map(bookSpans =>
        bookSpans.head.book.toLanguageString + " " + bookSpans.map(_.span.toLanguageString).mkString(", ")
      ).mkString("; ")

  override protected def getBook(name: String): Tanach.Prophets = Tanach.Prophets.forName(name)

  private def parseSources(value: Option[String]): Seq[String] = value
    .fold(Seq.empty[String])(_.split(',').toSeq)
    .map(_.trim).filter(_.nonEmpty)

  /** What is known about a reading besides the reading itself. */
  final case class Annotation(sources: Seq[String] = Nil, comment: Option[String] = None):
    def isEmpty: Boolean = sources.isEmpty && comment.isEmpty

    def ++(other: Annotation): Annotation = Annotation(
      sources = (sources ++ other.sources).distinct,
      comment = Seq(comment, other.comment).flatten.reduceOption((a, b) => s"$a $b")
    )

  private def annotation(sources: Option[String], comment: Option[String]): Annotation =
    // XML turns the newlines of a wrapped attribute value into spaces but does
    // not collapse the indentation that follows them.
    Annotation(parseSources(sources), comment.map(_.replaceAll("\\s+", " ").trim).filter(_.nonEmpty))

  type Annotations = Map[Custom, Annotation]

  /**
   * A reading in which a custom may read nothing at all: `None` is a value
   * here, not a missing entry, so a custom can read nothing where its parent
   * reads something. See `<none>`.
   */
  type OptionalCustoms = Custom.Of[Option[Haftarah]]

  /** An alternative reading for a custom, with whatever is known about it. */
  final case class Variant(number: Int, haftarah: Haftarah, annotation: Annotation) derives CanEqual

  type Variants = Map[Custom, Seq[Variant]]

  /** One `week` of Haftarah.xml, as read. */
  private final case class WeekMetadata(
    name: String,
    customs: Customs,
    annotations: Annotations,
    variants: Variants,
    precedenceWhenCombined: Set[Custom]
  )

  private lazy val loaded: Map[Parsha, WeekMetadata] =
    val parsed: Seq[WeekMetadata] = XmlParser.loadCatalog(this, WeekDto.codec).map(toWeekMetadata)
    Effects.unsafeRun(HasName.mapByName(
      keys = Parsha.valuesSeq,
      metadatas = parsed,
      hasName = (metadata: WeekMetadata, name: String) => metadata.name == name
    ))

  lazy val haftarah: Map[Parsha, Customs] = Collections.mapValues(loaded)(_.customs)

  /** Annotations per parsha and custom; parshiyos with none are absent. */
  lazy val annotationsByParsha: Map[Parsha, Annotations] =
    Collections.mapValues(loaded)(_.annotations).filter(_._2.nonEmpty)

  /** Readings recorded beside a custom's own; parshiyos with none are absent. */
  lazy val variantsByParsha: Map[Parsha, Variants] =
    Collections.mapValues(loaded)(_.variants).filter(_._2.nonEmpty)

  /** Customs for which this parsha's haftarah takes precedence when it is the
    * first of a combined week; empty for parshiyos that claim no precedence. */
  def precedenceWhenCombined(parsha: Parsha): Set[Custom] =
    loaded.get(parsha).fold(Set.empty)(_.precedenceWhenCombined)

  /** What an entry says about itself, for readings not keyed by parsha. */
  final case class Recorded(annotations: Annotations, variants: Variants):
    def isEmpty: Boolean = annotations.isEmpty && variants.isEmpty

  def decode[E: XmlAst](element: E, full: Boolean): Customs =
    XmlDecode.requireName(element, "haftarah")
    val parsed: Parsed = withAnnotations(HaftarahDto.codec.unsafeDecode(element), full)
    require(parsed.nones.isEmpty, "<none> in a reading that is not optional")
    parsed.customs

  def decodeRecorded[E: XmlAst](element: E, full: Boolean): Recorded =
    XmlDecode.requireName(element, "haftarah")
    val parsed: Parsed = withAnnotations(HaftarahDto.codec.unsafeDecode(element), full)
    Recorded(parsed.annotations, parsed.variants)

  /** A reading in which a custom may read nothing; see `<none>`. */
  def decodeOptional[E: XmlAst](element: E, full: Boolean): OptionalCustoms =
    XmlDecode.requireName(element, "haftarah")
    val parsed: Parsed = withAnnotations(HaftarahDto.codec.unsafeDecode(element), full = false)
    val reading: Map[Custom, Option[Haftarah]] =
      Collections.mapValues(parsed.customs.customs)(Some(_)) ++ parsed.nones.map(_ -> None)
    new Custom.Of[Option[Haftarah]](reading, full = full)

  private final case class Parsed(
    customs: Customs,
    annotations: Annotations,
    variants: Variants,
    nones: Set[Custom]
  )

  private def toWeekMetadata(dto: WeekDto): WeekMetadata =
    val forWeek: Annotation = annotation(dto.sources, dto.comment)
    val keep: Set[Custom] = dto.precedenceWhenCombined.map(_.trim).filter(_.nonEmpty)
      .fold(Set.empty)(Custom.parse)
    val result: Parsed = withAnnotations(dto.asHaftarah, full = true)
    // an annotation on the week itself stands for the entry as a whole
    val annotations: Annotations =
      if forWeek.isEmpty then result.annotations
      else result.annotations.updated(Custom.Common, result.annotations.getOrElse(Custom.Common, Annotation()) ++ forWeek)
    WeekMetadata(dto.n, result.customs, annotations, result.variants, keep)

  /**
   * `sources="michlol, chitas"`: the sources an entry rests on, by name; see
   * [[ReadingSources]]. Comma-separated, as `n` lists customs. Allowed on
   * `week` and on `custom`, not on `part`: the parts of one reading are
   * attested together.
   *
   * `comment="..."`: what the sources do not settle, said in words. For where
   * they disagree, or agree only with a qualification -- a chumash that prints
   * verses as "some add" is not the same as one that prints them plainly.
   * Allowed alongside `sources`, on `week` and on `custom`.
   *
   * `precedenceWhenCombined="Chabad"`: customs for which this parsha's
   * haftarah takes precedence when it is combined with the next, instead of
   * the second parsha's as combined weeks otherwise do. Names a custom and
   * everything under it, so `Common` means the whole tree.
   *
   * `variant="2"`: a reading recorded beside the custom's own, not instead of
   * it. For where the sources report a practice without settling who follows
   * it -- a variant is never resolved to, so nothing reads it by accident; it
   * is there to be shown next to the primary reading and to keep what is known
   * from being thrown away. Numbered from 2, the primary being 1.
   */
  private def withAnnotations(dto: HaftarahDto, full: Boolean): Parsed =
    val bookSpanParsed: BookSpanParsed = dto.span
    val parts: Seq[WithNumber[BookSpan]] = dto.parts.map(decodePart(bookSpanParsed, _))
    val partsOpt: Option[Haftarah] = if parts.isEmpty then None else Some(partsHaftarah(parts))
    val parsed: Seq[CustomParsed] = dto.customs.map(decodeCustom(bookSpanParsed, _))
    val standalone: Seq[(Set[Custom], Annotation)] = dto.annotations.map(decodeNamed)
    val noneEntries: Seq[(Set[Custom], Annotation)] = dto.nones.map(decodeNamed)
    // a variant stands beside the reading rather than being one of them, so it
    // takes no part in building the map that customs resolve through
    val (variantsParsed: Seq[CustomParsed], parsedCustoms: Seq[CustomParsed]) =
      parsed.partition(_.variant.isDefined)

    val variants: Variants = variantsParsed
      .flatMap(parsed => parsed.customs.toSeq.map(_ -> Variant(parsed.variant.get, parsed.haftarah, parsed.annotation)))
      .groupMap((custom, _) => custom)((_, variant) => variant)
      .view.mapValues(_.sortBy(_.number)).toMap

    val customsElements: Seq[(Set[Custom], Haftarah)] = parsedCustoms.map(p => (p.customs, p.haftarah))
    // one annotation may cover several customs: `<custom n="Sefard, Italki" sources="1"/>`
    val annotations: Annotations = (
      parsedCustoms
        .filterNot(_.annotation.isEmpty)
        .flatMap(parsed => parsed.customs.toSeq.map(_ -> parsed.annotation)) ++
      standalone
        .flatMap((customs, annotation) => customs.toSeq.map(_ -> annotation)) ++
      noneEntries
        .filterNot((_, annotation) => annotation.isEmpty)
        .flatMap((customs, annotation) => customs.toSeq.map(_ -> annotation))
    ).groupMapReduce((custom, _) => custom)((_, annotation) => annotation)(_ ++ _)

    val customs: Custom.Of[Haftarah] = Custom.Of(customsElements, full = false)
    val common: Option[Haftarah] = if parts.isEmpty && customsElements.isEmpty then Some(oneSpan(bookSpanParsed)) else partsOpt

    val result: Map[Custom, Haftarah] = common.fold(customs.customs)(common =>
      require(customs.find(Custom.Common).isEmpty)
      customs.customs.updated(Custom.Common, common)
    )

    Parsed(new Custom.Of(result, full = full), annotations, variants, noneEntries.flatMap(_._1).toSet)

  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  private final case class CustomParsed(
    customs: Set[Custom],
    haftarah: Haftarah,
    annotation: Annotation,
    variant: Option[Int]
  )

  private def decodeCustom(ancestorSpan: BookSpanParsed, dto: CustomDto): CustomParsed =
    val bookSpanParsed: BookSpanParsed = dto.span.inheritFrom(ancestorSpan)
    val parts: Seq[WithNumber[BookSpan]] = dto.parts.map(decodePart(bookSpanParsed, _))
    val result: Haftarah = if parts.isEmpty then oneSpan(bookSpanParsed) else partsHaftarah(parts)
    CustomParsed(Custom.parse(dto.n), result, annotation(dto.sources, dto.comment), dto.variant)

  /**
   * `<annotation n="Chabad" sources="chitas"/>`: what is known about a custom's
   * reading, where the custom has no reading of its own. A custom that follows
   * its parent has nowhere to carry a source, and giving it an entry to hold one
   * would assert a distinction that is not being made -- and be rejected, since
   * two entries cannot hold the same reading. This attaches the source to the
   * custom without touching what it reads.
   *
   * `<none n="Agadir"/>`: a custom that reads no haftarah at all. Absence
   * expressed by leaving a custom out of the map means "inherit from the
   * parent", so it cannot say this; and a hole is silent, where a value has to
   * be written down to exist. Only readings parsed as [[OptionalCustoms]] may
   * carry it -- the weekly readings are full, and everyone reads something.
   */
  private def decodeNamed(dto: NamedDto): (Set[Custom], Annotation) =
    (Custom.parse(dto.n), annotation(dto.sources, dto.comment))

  private def decodePart(ancestorSpan: BookSpanParsed, dto: PartDto): WithNumber[BookSpan] =
    WithNumber(dto.n, dto.span.inheritFrom(ancestorSpan).resolve)

  private def partsHaftarah(parts: Seq[WithNumber[BookSpan]]): Haftarah =
    WithNumber.requireConsecutive(parts, "part")
    require(parts.length > 1, "too short")
    Haftarah(WithNumber.dropNumbers(parts))

  private def spanOf(
    book: Option[String],
    fromChapter: Option[Int],
    fromVerse: Option[Int],
    toChapter: Option[Int],
    toVerse: Option[Int]
  ): BookSpanParsed = BookSpanParsed(
    book = book.map(_.trim).filter(_.nonEmpty),
    span = SpanParsed(VerseParsed(fromChapter, fromVerse), VerseParsed(toChapter, toVerse))
  )

  @Modifier.config(XmlCodec.Element, "haftarah")
  private final case class HaftarahDto(
    @Modifier.config(XmlCodec.Attribute, "") book: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None,
    @Modifier.config(XmlCodec.Element, "part") parts: Seq[PartDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "custom") customs: Seq[CustomDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "annotation") annotations: Seq[NamedDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "none") nones: Seq[NamedDto] = Seq.empty
  ) derives CanEqual:
    def span: BookSpanParsed = spanOf(book, fromChapter, fromVerse, toChapter, toVerse)

  private object HaftarahDto:
    given schema: Schema[HaftarahDto] = Schema.derived
    val codec: XmlCodec[HaftarahDto] = XmlCodec.derived

  @Modifier.config(XmlCodec.Element, "week")
  private final case class WeekDto(
    @Modifier.config(XmlCodec.Attribute, "") n: String,
    @Modifier.config(XmlCodec.Attribute, "") sources: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") comment: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") precedenceWhenCombined: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") book: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None,
    @Modifier.config(XmlCodec.Element, "part") parts: Seq[PartDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "custom") customs: Seq[CustomDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "annotation") annotations: Seq[NamedDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "none") nones: Seq[NamedDto] = Seq.empty
  ) derives CanEqual:
    def asHaftarah: HaftarahDto = HaftarahDto(
      book, fromChapter, fromVerse, toChapter, toVerse, parts, customs, annotations, nones
    )

  private object WeekDto:
    given schema: Schema[WeekDto] = Schema.derived
    val codec: XmlCodec[WeekDto] = XmlCodec.derived

  private final case class CustomDto(
    @Modifier.config(XmlCodec.Attribute, "") n: String,
    @Modifier.config(XmlCodec.Attribute, "") sources: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") comment: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") variant: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") book: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None,
    @Modifier.config(XmlCodec.Element, "part") parts: Seq[PartDto] = Seq.empty
  ) derives CanEqual:
    def span: BookSpanParsed = spanOf(book, fromChapter, fromVerse, toChapter, toVerse)

  private final case class PartDto(
    @Modifier.config(XmlCodec.Attribute, "") n: Int,
    @Modifier.config(XmlCodec.Attribute, "") book: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") fromVerse: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toChapter: Option[Int] = None,
    @Modifier.config(XmlCodec.Attribute, "") toVerse: Option[Int] = None
  ) derives CanEqual:
    def span: BookSpanParsed = spanOf(book, fromChapter, fromVerse, toChapter, toVerse)

  private final case class NamedDto(
    @Modifier.config(XmlCodec.Attribute, "") n: String,
    @Modifier.config(XmlCodec.Attribute, "") sources: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") comment: Option[String] = None
  ) derives CanEqual
