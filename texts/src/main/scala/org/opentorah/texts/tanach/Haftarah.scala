package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.metadata.{HasName, Language}
import org.podval.xml.{Xml, XmlCodec, XmlParser}
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
    // Wrapped <comment> text keeps its newlines and the indentation that follows them.
    Annotation(parseSources(sources), comment.map(_.replaceAll("\\s+", " ").trim).filter(_.nonEmpty))

  type Annotations = Map[Custom, Annotation]

  /**
   * A reading in which a custom may read nothing at all: `None` is a value
   * here, not a missing entry, so a custom can read nothing where its parent
   * reads something. See `reads="none"`.
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
    HasName.mapByName(
      keys = Parsha.valuesSeq,
      metadatas = parsed,
      hasName = (metadata: WeekMetadata, name: String) => metadata.name == name
    )

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

  def decode(element: Xml.Element, full: Boolean): Customs =
    XmlChecks.requireName(element, "haftarah")
    val parsed: Parsed = withAnnotations(HaftarahDto.codec.unsafeDecode(element), full)
    require(parsed.nones.isEmpty, """reads="none" in a reading that is not optional""")
    parsed.customs

  def decodeRecorded(element: Xml.Element, full: Boolean): Recorded =
    XmlChecks.requireName(element, "haftarah")
    val parsed: Parsed = withAnnotations(HaftarahDto.codec.unsafeDecode(element), full)
    Recorded(parsed.annotations, parsed.variants)

  /** A reading in which a custom may read nothing; see `reads="none"`. */
  def decodeOptional(element: Xml.Element, full: Boolean): OptionalCustoms =
    XmlChecks.requireName(element, "haftarah")
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
   * `week`, `custom` and `variant`, not on `span`: the spans of one reading
   * are attested together.
   *
   * `<comment>`: what the sources do not settle, said in words. Last child,
   * alongside `sources`, on `week`, `custom` and `variant`.
   *
   * `precedenceWhenCombined="Chabad"`: customs for which this parsha's
   * haftarah takes precedence when it is combined with the next, instead of
   * the second parsha's as combined weeks otherwise do. Names a custom and
   * everything under it, so `Common` means the whole tree.
   *
   * `reads="inherit"`: what is known about a custom's reading where the custom
   * has no reading of its own. A custom that follows its parent has nowhere to
   * carry a source, and giving it a span would assert a distinction that is
   * not being made -- and be rejected, since two entries cannot hold the same
   * reading.
   *
   * `reads="none"`: a custom that reads no haftarah at all. Absence expressed
   * by leaving a custom out of the map means "inherit from the parent", so it
   * cannot say this. Only readings parsed as [[OptionalCustoms]] may carry it
   * -- the weekly readings are full, and everyone reads something.
   *
   * Nested `<variant>`: a reading recorded beside the custom's own, not
   * instead of it. Never resolved to. Numbered from 2 in document order, the
   * primary being 1. `n` on variant names a subset of the parent custom;
   * omitted, the variant belongs to every custom on the parent.
   */
  private def withAnnotations(dto: HaftarahDto, full: Boolean): Parsed =
    val ancestor: BookSpanParsed = bookOnly(dto.book)
    val parsed: Seq[CustomParsed] = dto.customs.map(decodeCustom(ancestor, _))
    val readings: Seq[CustomParsed] = parsed.filter(_.reads.isEmpty)
    val inherit: Seq[CustomParsed] = parsed.filter(_.reads.contains("inherit"))
    val noneEntries: Seq[CustomParsed] = parsed.filter(_.reads.contains("none"))

    val variants: Variants = readings
      .flatMap(_.variants)
      .groupMap((custom, _) => custom)((_, variant) => variant)
      .view.mapValues(_.sortBy(_.number)).toMap

    val customsElements: Seq[(Set[Custom], Haftarah)] = readings.map(p => (p.customs, p.haftarah.get))
    val annotations: Annotations = (
      (readings ++ inherit ++ noneEntries)
        .filterNot(_.annotation.isEmpty)
        .flatMap(parsed => parsed.customs.toSeq.map(_ -> parsed.annotation))
    ).groupMapReduce((custom, _) => custom)((_, annotation) => annotation)(_ ++ _)

    val customs: Custom.Of[Haftarah] = Custom.Of(customsElements, full = false)
    Parsed(new Custom.Of(customs.customs, full = full), annotations, variants, noneEntries.flatMap(_.customs).toSet)

  private def bookOnly(book: Option[String]): BookSpanParsed = BookSpanParsed(
    book = book.map(_.trim).filter(_.nonEmpty),
    span = SpanParsed(VerseParsed(None, None), VerseParsed(None, None))
  )

  private final case class CustomParsed(
    customs: Set[Custom],
    haftarah: Option[Haftarah],
    annotation: Annotation,
    variants: Seq[(Custom, Variant)],
    reads: Option[String]
  )

  private def decodeCustom(ancestor: BookSpanParsed, dto: CustomDto): CustomParsed =
    val reads: Option[String] = parseReads(dto.reads)
    val here: BookSpanParsed = bookOnly(dto.book).inheritFrom(ancestor)
    val note: Annotation = annotation(dto.sources, dto.comment)
    val names: Set[Custom] = Custom.parse(dto.n)
    reads match
      case Some(_) =>
        require(dto.spans.isEmpty, s"custom '${dto.n}' with reads= cannot have span")
        require(dto.variants.isEmpty, s"custom '${dto.n}' with reads= cannot have variant")
        CustomParsed(names, None, note, Nil, reads)
      case None =>
        require(dto.spans.nonEmpty, s"custom '${dto.n}' needs a span")
        CustomParsed(
          names,
          Some(spansHaftarah(dto.spans.map(decodeSpan(here, _)))),
          note,
          dto.variants.zipWithIndex.flatMap((variant, index) =>
            decodeVariant(here, names, variant, index + 2)),
          None
        )

  private def parseReads(value: Option[String]): Option[String] =
    value.map(_.trim).filter(_.nonEmpty).map: v =>
      require(v == "inherit" || v == "none", s"reads must be inherit or none, got '$v'")
      v

  private def decodeVariant(
    ancestor: BookSpanParsed,
    parent: Set[Custom],
    dto: VariantDto,
    number: Int
  ): Seq[(Custom, Variant)] =
    require(dto.spans.nonEmpty, "variant needs a span")
    val customs: Set[Custom] = dto.n.fold(parent): n =>
      val subset: Set[Custom] = Custom.parse(n)
      require(subset.subsetOf(parent), s"variant n='$n' is not under '${parent.map(_.name).mkString(", ")}'")
      subset
    val variant: Variant =
      Variant(number, spansHaftarah(dto.spans.map(decodeSpan(ancestor, _))), annotation(dto.sources, dto.comment))
    customs.toSeq.map(_ -> variant)

  private def decodeSpan(ancestor: BookSpanParsed, dto: SpanDto): BookSpan =
    dto.parsed.inheritFrom(ancestor).resolve

  private def spansHaftarah(spans: Seq[BookSpan]): Haftarah =
    require(spans.nonEmpty, "empty reading")
    Haftarah(spans)

  @Modifier.config(XmlCodec.Element, "haftarah")
  private final case class HaftarahDto(
    book: Option[String] = None,
    when: Option[String] = None,
    role: Option[String] = None,
    n: Option[Int] = None,
    partial: Option[Boolean] = None,
    customs: Seq[CustomDto] = Seq.empty
  ) derives CanEqual

  private object HaftarahDto:
    given schema: Schema[HaftarahDto] = Schema.derived
    val codec: XmlCodec[HaftarahDto] = XmlCodec.derived

  @Modifier.config(XmlCodec.Element, "week")
  private final case class WeekDto(
    n: String,
    sources: Option[String] = None,
    precedenceWhenCombined: Option[String] = None,
    book: Option[String] = None,
    customs: Seq[CustomDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "comment") comment: Option[String] = None
  ) derives CanEqual:
    def asHaftarah: HaftarahDto = HaftarahDto(book = book, customs = customs)

  private object WeekDto:
    given schema: Schema[WeekDto] = Schema.derived
    val codec: XmlCodec[WeekDto] = XmlCodec.derived

  @Modifier.config(XmlCodec.Element, "custom")
  private final case class CustomDto(
    n: String,
    sources: Option[String] = None,
    reads: Option[String] = None,
    book: Option[String] = None,
    spans: Seq[SpanDto] = Seq.empty,
    variants: Seq[VariantDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "comment") comment: Option[String] = None
  ) derives CanEqual

  private object CustomDto:
    given schema: Schema[CustomDto] = Schema.derived
    val codec: XmlCodec[CustomDto] = XmlCodec.derived

  @Modifier.config(XmlCodec.Element, "variant")
  private final case class VariantDto(
    n: Option[String] = None,
    sources: Option[String] = None,
    spans: Seq[SpanDto] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "comment") comment: Option[String] = None
  ) derives CanEqual

  private object VariantDto:
    given schema: Schema[VariantDto] = Schema.derived
    val codec: XmlCodec[VariantDto] = XmlCodec.derived

  @Modifier.config(XmlCodec.Element, "span")
  private final case class SpanDto(
    book: Option[String] = None,
    from: Option[String] = None,
    to: Option[String] = None
  ) derives CanEqual:
    def parsed: BookSpanParsed = BookSpanParsed(
      book = book.map(_.trim).filter(_.nonEmpty),
      span = SpanParsed(VerseParsed.parseOpt(from), VerseParsed.parseOpt(to))
    )

  private object SpanDto:
    given schema: Schema[SpanDto] = Schema.derived
    val codec: XmlCodec[SpanDto] = XmlCodec.derived

