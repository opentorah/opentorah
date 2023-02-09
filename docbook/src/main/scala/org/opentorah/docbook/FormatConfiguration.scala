package org.opentorah.docbook

import org.opentorah.math.MathConfiguration
import org.opentorah.util.Strings
import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

final class FormatConfiguration(
  name: String,
  val parameters: Map[String, String],
  val math: Option[MathConfiguration],
  val xslt1version: Option[String],
  val xslt2version: Option[String],
  val epubEmbeddedFonts: List[String],
  val variants: Set[VariantConfiguration]
):
  val format: Format = Format.forName(name)

  def toVariants: Set[Variant] = Set(toVariant(None)) ++
    (for variant: VariantConfiguration <- variants yield toVariant(Some(variant)))

  private def toVariant(variant: Option[VariantConfiguration]): Variant = Variant(
    format = format,
    parameters = parameters,
    math = math,
    xslt1version = xslt1version,
    xslt2version = xslt2version,
    epubEmbeddedFonts = epubEmbeddedFonts,
    configuration = variant
  )

object FormatConfiguration extends Element[FormatConfiguration]("format"):

  override def contentParsable: Parsable[FormatConfiguration] = new Parsable[FormatConfiguration]:

    override def parser: Parser[FormatConfiguration] = for
      name: String <- Configuration.nameAttribute()
      parameters: Seq[ParameterConfiguration] <- ParameterConfiguration.seq()
      math: Option[MathConfiguration] <- MathConfiguration.optional()
      xslt1version: Option[String] <- Configuration.xslt1versionAttribute()
      xslt2version: Option[String] <- Configuration.xslt2versionAttribute()
      epubEmbeddedFonts: Option[String] <- Configuration.epubEmbeddedFontsAttribute()
      variants: Seq[VariantConfiguration] <- VariantConfiguration.seq()
    yield FormatConfiguration(
      name = name,
      parameters = ParameterConfiguration.toMap(parameters),
      math = math,
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = Strings.toList(epubEmbeddedFonts),
      variants = variants.toSet
    )

    override def unparser: Unparser[FormatConfiguration] = Unparser.concat[FormatConfiguration](
      Configuration.nameAttribute(_.format.name),
      ParameterConfiguration.seq(configuration => ParameterConfiguration.fromMap(configuration.parameters)),
      MathConfiguration.optional(_.math),
      Configuration.xslt1versionAttribute(_.xslt1version),
      Configuration.xslt1versionAttribute(_.xslt2version),
      Configuration.epubEmbeddedFontsAttribute(format => Strings.fromListOption(format.epubEmbeddedFonts)),
      VariantConfiguration.seq(_.variants.toSeq)
    )
