package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, Language, Names}
import org.opentorah.util.{Collections, Effects}
import org.opentorah.xml.{Attribute, ElementTo, From, Parsable, Parser, Unparser}
import zio.ZIO

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

  /**
   * The sources an entry rests on, by name: `sources="michlol, chitas"`; see
   * [[ReadingSources]]. Comma-separated, as `n` lists customs. Allowed on
   * `week` and on `custom`, not on `part`: the parts of one reading are
   * attested together.
   */
  private val sourcesAttribute: Attribute[String] = Attribute("sources")

  /**
   * What the sources do not settle, said in words: `comment="..."`. For where
   * they disagree, or agree only with a qualification -- a chumash that prints
   * verses as "some add" is not the same as one that prints them plainly.
   * Allowed alongside `sources`, on `week` and on `custom`.
   */
  private val commentAttribute: Attribute[String] = Attribute("comment")

  /**
   * Customs for which this parsha's haftarah takes precedence when it is
   * combined with the next, instead of the second parsha's as combined weeks
   * otherwise do: `precedenceWhenCombined="Chabad"`. Names a custom and
   * everything under it, so `Common` means the whole tree.
   */
  private val precedenceWhenCombinedAttribute: Attribute[String] = Attribute("precedenceWhenCombined")

  /**
   * A reading recorded beside the custom's own, not instead of it:
   * `variant="2"`. For where the sources report a practice without settling
   * who follows it -- a variant is never resolved to, so nothing reads it by
   * accident; it is there to be shown next to the primary reading and to keep
   * what is known from being thrown away. Numbered from 2, the primary being 1.
   */
  private val variantAttribute: Attribute[String] = Attribute("variant")

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
   * reads something. See [[NoneElement]].
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

  private object Week extends ElementTo[WeekMetadata]("week"):
    private val elementParser = Haftarah.parserWithAnnotations(full = true)

    override def contentParsable: Parsable[WeekMetadata] = new Parsable[WeekMetadata]:
      override def parser: Parser[WeekMetadata] = for
        name: String <- Names.defaultNameAttribute.required()
        weekSources: Option[String] <- sourcesAttribute.optional()
        weekComment: Option[String] <- commentAttribute.optional()
        keep: Option[String] <- precedenceWhenCombinedAttribute.optional()
        result: (Customs, Annotations, Variants, Set[Custom]) <- elementParser
      yield
        val forWeek: Annotation = annotation(weekSources, weekComment)
        // an annotation on the week itself stands for the entry as a whole
        val annotations: Annotations =
          if forWeek.isEmpty then result._2
          else result._2.updated(Custom.Common, result._2.getOrElse(Custom.Common, Annotation()) ++ forWeek)
        WeekMetadata(name, result._1, annotations, result._3, keep.fold(Set.empty)(Custom.parse))

      override def unparser: Unparser[WeekMetadata] = ???

  private lazy val loaded: Map[Parsha, WeekMetadata] = Parser.unsafeRun(HasName.load(
    from = From.resource(this),
    content = Week,
    keys = Parsha.valuesSeq,
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

  /** The annotations of one `<haftarah>`, for callers that key it themselves. */
  def recordedIn(full: Boolean): ElementTo[Recorded] = new ElementTo[Recorded]("haftarah"):
    override def contentParsable: Parsable[Recorded] = new Parsable[Recorded]:
      override def parser: Parser[Recorded] =
        Haftarah.parserWithAnnotations(full).map((_, annotations, variants, _) =>
          Recorded(annotations, variants))

      override def unparser: Unparser[Recorded] = ???

  def element(full: Boolean): ElementTo[Customs] = new ElementTo[Customs]("haftarah"):
    override def contentParsable: Parsable[Customs] = new Parsable[Customs]:
      override def parser: Parser[Customs] = Haftarah.parserWithAnnotations(full).map(parsed =>
        require(parsed._4.isEmpty, "<none> in a reading that is not optional")
        parsed._1
      )

      override def unparser: Unparser[Haftarah.Customs] = ???

  /** A reading in which a custom may read nothing; see [[NoneElement]]. */
  def optionalElement(full: Boolean): ElementTo[OptionalCustoms] =
    new ElementTo[OptionalCustoms]("haftarah"):
      override def contentParsable: Parsable[OptionalCustoms] = new Parsable[OptionalCustoms]:
        override def parser: Parser[OptionalCustoms] =
          Haftarah.parserWithAnnotations(full = false).map((customs, _, _, nones) =>
            val reading: Map[Custom, Option[Haftarah]] =
              Collections.mapValues(customs.customs)(Some(_)) ++ nones.map(_ -> None)
            new Custom.Of[Option[Haftarah]](reading, full = full)
          )

        override def unparser: Unparser[OptionalCustoms] = ???

  private def parserWithAnnotations(full: Boolean): Parser[(Customs, Annotations, Variants, Set[Custom])] = for
    bookSpanParsed: BookSpanParsed <- spanParser
    parts: Seq[WithNumber[BookSpan]] <- PartElement(bookSpanParsed).seq()
    partsOpt: Option[Haftarah] <- if parts.isEmpty then ZIO.none else partsParser(parts).map(Some(_))
    parsed: Seq[CustomParsed] <- CustomElement(bookSpanParsed).seq()
    standalone: Seq[(Set[Custom], Annotation)] <- AnnotationElement.seq()
    nones: Seq[(Set[Custom], Annotation)] <- NoneElement.seq()
  yield
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
      nones
        .filterNot((_, annotation) => annotation.isEmpty)
        .flatMap((customs, annotation) => customs.toSeq.map(_ -> annotation))
    ).groupMapReduce((custom, _) => custom)((_, annotation) => annotation)(_ ++ _)

    val customs: Custom.Of[Haftarah] = Custom.Of(customsElements, full = false)
    val common: Option[Haftarah] = if parts.isEmpty && customsElements.isEmpty then Some(oneSpan(bookSpanParsed)) else partsOpt

    val result: Map[Custom, Haftarah] = common.fold(customs.customs)(common =>
      require(customs.find(Custom.Common).isEmpty)
      customs.customs.updated(Custom.Common, common)
    )

    (new Custom.Of(result, full = full), annotations, variants, nones.flatMap(_._1).toSet)

  private def oneSpan(span: BookSpanParsed): Haftarah = Haftarah(Seq(span.resolve))

  private final case class CustomParsed(
    customs: Set[Custom],
    haftarah: Haftarah,
    annotation: Annotation,
    variant: Option[Int]
  )

  private final class CustomElement(ancestorSpan: BookSpanParsed)
    extends ElementTo[CustomParsed]("custom"):
    override def contentParsable: Parsable[CustomParsed] =
      new Parsable[CustomParsed]:
        override def parser: Parser[CustomParsed] = for
          n: String <- Attribute("n").required()
          sources: Option[String] <- sourcesAttribute.optional()
          comment: Option[String] <- commentAttribute.optional()
          variant: Option[String] <- variantAttribute.optional()
          bookSpanParsed: BookSpanParsed <- spanParser.map(_.inheritFrom(ancestorSpan))
          parts: Seq[WithNumber[BookSpan]] <- PartElement(bookSpanParsed).seq()
          result: Haftarah <- if parts.isEmpty then ZIO.succeed[Haftarah](oneSpan(bookSpanParsed)) else partsParser(parts)
        yield CustomParsed(Custom.parse(n), result, annotation(sources, comment), variant.map(_.trim.toInt))

        override def unparser: Unparser[CustomParsed] = ???

  /**
   * What is known about a custom's reading, where the custom has no reading of
   * its own: `<annotation n="Chabad" sources="chitas"/>`. A custom that follows
   * its parent has nowhere to carry a source, and giving it an entry to hold one
   * would assert a distinction that is not being made -- and be rejected, since
   * two entries cannot hold the same reading. This attaches the source to the
   * custom without touching what it reads.
   */
  /**
   * A custom that reads no haftarah at all: `<none n="Agadir"/>`. Absence
   * expressed by leaving a custom out of the map means "inherit from the
   * parent", so it cannot say this; and a hole is silent, where a value has to
   * be written down to exist. Only readings parsed as [[OptionalCustoms]] may
   * carry it -- the weekly readings are full, and everyone reads something.
   */
  private object NoneElement extends ElementTo[(Set[Custom], Annotation)]("none"):
    override def contentParsable: Parsable[(Set[Custom], Annotation)] =
      new Parsable[(Set[Custom], Annotation)]:
        override def parser: Parser[(Set[Custom], Annotation)] = for
          n: String <- Attribute("n").required()
          sources: Option[String] <- sourcesAttribute.optional()
          comment: Option[String] <- commentAttribute.optional()
        yield (Custom.parse(n), annotation(sources, comment))

        override def unparser: Unparser[(Set[Custom], Annotation)] = ???

  private object AnnotationElement extends ElementTo[(Set[Custom], Annotation)]("annotation"):
    override def contentParsable: Parsable[(Set[Custom], Annotation)] =
      new Parsable[(Set[Custom], Annotation)]:
        override def parser: Parser[(Set[Custom], Annotation)] = for
          n: String <- Attribute("n").required()
          sources: Option[String] <- sourcesAttribute.optional()
          comment: Option[String] <- commentAttribute.optional()
        yield (Custom.parse(n), annotation(sources, comment))

        override def unparser: Unparser[(Set[Custom], Annotation)] = ???

  private final class PartElement(ancestorSpan: BookSpanParsed) extends ElementTo[WithNumber[BookSpan]]("part"):
    override def contentParsable: Parsable[WithNumber[BookSpan]] = new Parsable[WithNumber[BookSpan]]:
      override def parser: Parser[WithNumber[BookSpan]] =
        WithNumber.parse(spanParser.map(_.inheritFrom(ancestorSpan).resolve))

      override def unparser: Unparser[WithNumber[BookSpan]] = ???

  private def partsParser(parts: Seq[WithNumber[BookSpan]]): Parser[Haftarah] = for
    _ <- WithNumber.checkConsecutive(parts, "part")
    _ <- Effects.check(parts.length > 1, "too short")
  yield Haftarah(WithNumber.dropNumbers(parts))
