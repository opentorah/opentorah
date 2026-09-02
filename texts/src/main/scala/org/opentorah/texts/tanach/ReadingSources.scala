package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.xml.{XmlCodec, XmlParser}
import zio.blocks.schema.{Modifier, Schema}

/**
 * Where a reading is attested. Readings differ between customs, and the
 * differences are the part most likely to be questioned, so an entry in
 * Haftarah.xml can name the sources it rests on:
 *
 *   <week n="Vayeilech" sources="michlol chitas">
 *     <custom n="Ashkenaz" sources="2"> ... </custom>
 *   </week>
 *
 * A `sources` on a
 * `custom` applies to that custom; one on the `week` applies to the whole
 * entry, and both are reported for a custom that carries its own.
 *
 * The readings themselves are untouched: nothing here changes what is read,
 * only what can be said about where it comes from. Ask via [[forParsha]].
 *
 * Entries are added as readings are checked; an unannotated reading means
 * "not yet traced", not "unsourced".
 */
object ReadingSources:

  enum Kind derives CanEqual:
    case Chumash    // printed chumash or tikkun
    case Siddur     // siddur or machzor
    case Halachah   // halachic work
    case Study      // scholarly edition, article or manuscript
    case Website
    /** Not a work of its own: a reading established by combining others. */
    case Reconstruction

  object Kind:
    def parse(value: String): Kind = Kind.values
      .find(_.toString.equalsIgnoreCase(value))
      .getOrElse(throw IllegalArgumentException(
        s"Unknown source kind: '$value'; known: ${Kind.values.map(_.toString.toLowerCase).mkString(", ")}"))

  final case class Source(
    /** How entries refer to this source: lowercase, hyphenated, unique. */
    key: String,
    kind: Kind,
    name: String,
    /** Imprint of a printed work: publisher, place, year. */
    publication: Option[String] = None,
    /** Where within it -- an article, page, or siman. */
    where: Option[String] = None,
    url: Option[String] = None,
    /** For Kind.Reconstruction: the sources it is built from. */
    combines: Seq[String] = Nil
  ):
    override def toString: String =
      name +
      where.fold("")(w => s", $w") +
      publication.fold("")(pub => s" ($pub)") +
      url.fold("")(u => s" <$u>")

  private final case class ParsedSource(
    @Modifier.config(XmlCodec.Attribute, "n") key: String,
    @Modifier.config(XmlCodec.Attribute, "") kind: String,
    @Modifier.config(XmlCodec.Attribute, "") name: String,
    @Modifier.config(XmlCodec.Attribute, "") publication: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") where: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") url: Option[String] = None,
    @Modifier.config(XmlCodec.Attribute, "") combines: Option[String] = None
  ) derives CanEqual

  private object ParsedSource:
    given schema: Schema[ParsedSource] = Schema.derived
    val codec: XmlCodec[ParsedSource] = XmlCodec.derived

    def toSource(parsed: ParsedSource): Source = Source(
      key = parsed.key,
      kind = Kind.parse(parsed.kind),
      name = parsed.name,
      publication = parsed.publication,
      where = parsed.where,
      url = parsed.url,
      combines = parsed.combines.fold(Seq.empty)(_.split(',').toSeq.map(_.trim).filter(_.nonEmpty))
    )

  /** The works themselves, read from ReadingSources.xml. */
  lazy val sources: Map[String, Source] =
    val parsed: Seq[Source] = XmlParser.loadCatalog(this, ParsedSource.codec).map(ParsedSource.toSource)
    Collections.checkNoDuplicates(parsed.map(_.key), "sources")
    val byKey: Map[String, Source] = parsed.map(source => source.key -> source).toMap
    for
      source <- parsed
      combined <- source.combines
    do require(byKey.contains(combined),
      s"Source '${source.key}' combines '$combined', which is not a source")
    byKey

  /** What is recorded for this parsha, for this custom: the custom's own,
    * then whatever is given for the entry as a whole. */
  private def annotationFor(parsha: Parsha, custom: Custom): Haftarah.Annotation =
    val byCustom: Map[Custom, Haftarah.Annotation] =
      Haftarah.annotationsByParsha.getOrElse(parsha, Map.empty)
    byCustom.getOrElse(custom, Haftarah.Annotation()) ++
      byCustom.getOrElse(Custom.Common, Haftarah.Annotation())

  /** The source a `sources` entry refers to; an unknown key is a data error. */
  def byKey(key: String): Source = sources.getOrElse(key, throw IllegalArgumentException(
    s"No such source: '$key'; known: ${sources.keys.toSeq.sorted.mkString(", ")}"))

  /** Sources named for this parsha and custom. Empty when nothing is recorded. */
  def forParsha(parsha: Parsha, custom: Custom): Seq[Source] =
    annotationFor(parsha, custom).sources.map(byKey)

  /** What the sources do not settle between them, where that was written down. */
  def commentFor(parsha: Parsha, custom: Custom): Option[String] =
    annotationFor(parsha, custom).comment

  /** Readings recorded beside this custom's own, where the sources report a
    * practice without settling who follows it. */
  def variantsFor(parsha: Parsha, custom: Custom): Seq[Haftarah.Variant] =
    Haftarah.variantsByParsha.getOrElse(parsha, Map.empty).getOrElse(custom, Nil)

  /** What is recorded for one of the special readings, which are keyed by the
    * day and the reading name rather than by parsha. */
  private def specialAnnotation(day: String, reading: String, custom: Custom): Haftarah.Annotation =
    val byCustom: Map[Custom, Haftarah.Annotation] =
      SpecialReadings.recorded.get((day, reading)).fold(Map.empty)(_.annotations)
    byCustom.getOrElse(custom, Haftarah.Annotation()) ++
      byCustom.getOrElse(Custom.Common, Haftarah.Annotation())

  def forSpecialReading(day: String, reading: String, custom: Custom): Seq[Source] =
    specialAnnotation(day, reading, custom).sources.map(byKey)

  def commentForSpecialReading(day: String, reading: String, custom: Custom): Option[String] =
    specialAnnotation(day, reading, custom).comment

  def variantsForSpecialReading(day: String, reading: String, custom: Custom): Seq[Haftarah.Variant] =
    SpecialReadings.recorded.get((day, reading)).fold(Nil)(_.variants.getOrElse(custom, Nil))

  /** Every parsha that names at least one source or carries a comment. */
  def annotatedParshiyos: Set[Parsha] = Haftarah.annotationsByParsha.keySet
