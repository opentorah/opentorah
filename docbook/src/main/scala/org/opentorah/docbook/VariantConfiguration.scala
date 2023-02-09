package org.opentorah.docbook

import org.opentorah.math.MathConfiguration
import org.opentorah.util.Strings
import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

final class VariantConfiguration(
  val name: String,
  val parameters: Map[String, String],
  val math: Option[MathConfiguration],
  val xslt1version: Option[String],
  val xslt2version: Option[String],
  val epubEmbeddedFonts: List[String]
):
  require(name.nonEmpty)

object VariantConfiguration extends Element[VariantConfiguration]("variant"):

  override def contentParsable: Parsable[VariantConfiguration] = new Parsable[VariantConfiguration]:

    override def parser: Parser[VariantConfiguration] = for
      name: String <- Configuration.nameAttribute()
      parameters: Seq[ParameterConfiguration] <- ParameterConfiguration.seq()
      math: Option[MathConfiguration] <- MathConfiguration.optional()
      xslt1version: Option[String] <- Configuration.xslt1versionAttribute()
      xslt2version: Option[String] <- Configuration.xslt2versionAttribute()
      epubEmbeddedFonts: Option[String] <- Configuration.epubEmbeddedFontsAttribute()
    yield VariantConfiguration(
      name = name,
      parameters = ParameterConfiguration.toMap(parameters),
      math = math,
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = Strings.toList(epubEmbeddedFonts)
    )

    override def unparser: Unparser[VariantConfiguration] = Unparser.concat[VariantConfiguration](
      Configuration.nameAttribute(_.name),
      ParameterConfiguration.seq(configuration => ParameterConfiguration.fromMap(configuration.parameters)),
      MathConfiguration.optional(_.math),
      Configuration.xslt1versionAttribute(_.xslt1version),
      Configuration.xslt1versionAttribute(_.xslt2version),
      Configuration.epubEmbeddedFontsAttribute(variant => Strings.fromListOption(variant.epubEmbeddedFonts))
    )
