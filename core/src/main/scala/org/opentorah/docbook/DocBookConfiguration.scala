package org.opentorah.docbook

import org.opentorah.html.SiteHtml
import org.opentorah.math.MathConfiguration
import org.opentorah.util.{BuildContext, Strings}
import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

final class DocBookConfiguration(
  val output: Set[String],
  val math: Option[MathConfiguration],
  val substitutions: Map[String, String],
  val xslt1version: Option[String],
  val xslt2version: Option[String],
  val epubEmbeddedFonts: List[String],
  val documents: Set[DocumentConfiguration],
  val common: Set[CommonConfiguration],
  val formats: Set[FormatConfiguration]
):
  def isEmpty: Boolean =
    output.isEmpty &&
    (math.isEmpty || math.get.isEmpty) &&
    substitutions.isEmpty &&
    epubEmbeddedFonts.isEmpty &&
    documents.isEmpty &&
    common.isEmpty &&
    formats.isEmpty

  def toProcessor(
    layout: Layout,
    context: BuildContext,
    siteHtml: SiteHtml
  ): DocBookProcessor =

    val variants: Set[Variant] = formats.flatMap(_.toVariants)

    new DocBookProcessor(
      layout,
      context,
      siteHtml,
      outputDefault = Variant.forNames(output, variants),
      mathDefault = math,
      substitutionsDefault = substitutions,
      xslt1versionDefault = xslt1version,
      xslt2versionDefault = xslt2version,
      epubEmbeddedFontsDefault = epubEmbeddedFonts,
      documents = for document <- documents yield document.toDocument(variants),
      common2parameters = (for common <- common yield (common.common, common.parameters)).toMap,
      variants = variants
    )

object DocBookConfiguration extends Element[DocBookConfiguration]("docbook"):

  override def contentParsable: Parsable[DocBookConfiguration] = new Parsable[DocBookConfiguration]:

    override def parser: Parser[DocBookConfiguration] = for
      output: Seq[OutputConfiguration] <- OutputConfiguration.seq()
      math: Option[MathConfiguration] <- MathConfiguration.optional()
      epubEmbeddedFonts: Option[String] <- Configuration.epubEmbeddedFontsAttribute()
      xslt1version: Option[String] <- Configuration.xslt1versionAttribute()
      xslt2version: Option[String] <- Configuration.xslt2versionAttribute()
      substitutions: Seq[SubstitutionConfiguration] <- SubstitutionConfiguration.seq()
      documents: Seq[DocumentConfiguration] <- DocumentConfiguration.seq()
      common: Seq[CommonConfiguration] <- CommonConfiguration.seq()
      formats: Seq[FormatConfiguration] <- FormatConfiguration.seq()
    yield DocBookConfiguration(
      output = OutputConfiguration.toSet(output),
      math = math,
      substitutions = SubstitutionConfiguration.toMap(substitutions),
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = Strings.toList(epubEmbeddedFonts),
      documents = documents.toSet,
      common = common.toSet,
      formats = formats.toSet
    )

    override def unparser: Unparser[DocBookConfiguration] = Unparser.concat[DocBookConfiguration](
      OutputConfiguration.seq(configuration => OutputConfiguration.fromSet(configuration.output)),
      MathConfiguration.optional(_.math),
      SubstitutionConfiguration.seq(configuration => SubstitutionConfiguration.fromMap(configuration.substitutions)),
      Configuration.xslt1versionAttribute(_.xslt1version),
      Configuration.xslt1versionAttribute(_.xslt2version),
      Configuration.epubEmbeddedFontsAttribute(configuration => Strings.fromListOption(configuration.epubEmbeddedFonts)),
      DocumentConfiguration.seq(_.documents.toSeq),
      CommonConfiguration.seq(_.common.toSeq),
      FormatConfiguration.seq(_.formats.toSeq)
    )
