package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, ElementTo, From, Parsable, Parser, Unparser}

/**
 * Where a reading is attested. Readings differ between customs, and the
 * differences are the part most likely to be questioned, so an entry in
 * Haftarah.xml can name the sources it rests on:
 *
 *   <week n="Vayeilech" sources="1 2">
 *     <custom n="Ashkenaz" sources="2"> ... </custom>
 *   </week>
 *
 * The numbers are indices into [[ReadingSources.sources]]. A `sources` on a
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

  private object SourceElement extends ElementTo[Source]("source"):
    override def contentParsable: Parsable[Source] = new Parsable[Source]:
      override def parser: Parser[Source] = for
        key: String <- Attribute("n").required()
        kind: String <- Attribute("kind").required()
        name: String <- Attribute("name").required()
        publication: Option[String] <- Attribute("publication").optional()
        where: Option[String] <- Attribute("where").optional()
        url: Option[String] <- Attribute("url").optional()
        combines: Option[String] <- Attribute("combines").optional()
      yield Source(
        key = key,
        kind = Kind.parse(kind),
        name = name,
        publication = publication,
        where = where,
        url = url,
        combines = combines.fold(Seq.empty)(_.split(',').toSeq.map(_.trim).filter(_.nonEmpty))
      )

      override def unparser: Unparser[Source] = ???

  /** The works themselves, read from ReadingSources.xml. */
  lazy val sources: Map[String, Source] =
    val from: From = From.resourceNamed(this, "ReadingSources")
    val parsed: Seq[Source] = Parser.unsafeRun(SourceElement.wrappedSeq(from.name).parse(from))
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

  /** Every parsha that names at least one source or carries a comment. */
  def annotatedParshiyos: Set[Parsha] = Haftarah.annotationsByParsha.keySet
