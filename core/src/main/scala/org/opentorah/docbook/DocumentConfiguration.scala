package org.opentorah.docbook

import org.opentorah.math.MathConfiguration
import org.opentorah.util.{Files, Strings}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class DocumentConfiguration(
  val name: String,
  val output: Set[String],
  val parameters: Map[String, String],
  val math: Option[MathConfiguration],
  val substitutions: Map[String, String],
  val xslt1version: Option[String],
  val xslt2version: Option[String],
  val epubEmbeddedFonts: List[String],
  val dataGeneratorClass: Option[String],
  val imagesDirectory: Option[String]
):
  def toDocument(variants: Set[Variant]): Document = Document(
    name = Files.dropAllowedExtension(name, "xml"),
    output = Variant.forNames(output, variants),
    parameters = parameters,
    math = math,
    substitutions = substitutions,
    xslt1version = xslt1version,
    xslt2version = xslt2version,
    epubEmbeddedFonts = epubEmbeddedFonts,
    dataGeneratorClass = dataGeneratorClass,
    imagesDirectory = imagesDirectory
  )

object DocumentConfiguration extends Element[DocumentConfiguration]("document"):
  override def contentParsable: Parsable[DocumentConfiguration] = new Parsable[DocumentConfiguration]:
    private val dataGeneratorClassAttribute: Attribute.Optional[String] = Attribute("dataGeneratorClass").optional
    private val imagesDirectoryAttribute: Attribute.Optional[String] = Attribute("imagesDirectory").optional

    override def parser: Parser[DocumentConfiguration] = for
      name: String <- Configuration.nameAttribute()
      output: Seq[OutputConfiguration] <- OutputConfiguration.seq()
      parameters: Seq[ParameterConfiguration] <- ParameterConfiguration.seq()
      math: Option[MathConfiguration] <- MathConfiguration.optional()
      substitutions: Seq[SubstitutionConfiguration] <- SubstitutionConfiguration.seq()
      xslt1version: Option[String] <- Configuration.xslt1versionAttribute()
      xslt2version: Option[String] <- Configuration.xslt2versionAttribute()
      epubEmbeddedFonts: Option[String] <- Configuration.epubEmbeddedFontsAttribute()
      dataGeneratorClass: Option[String] <- dataGeneratorClassAttribute()
      imagesDirectory: Option[String] <- imagesDirectoryAttribute()
    yield DocumentConfiguration(
      name = name,
      output = OutputConfiguration.toSet(output),
      parameters = ParameterConfiguration.toMap(parameters),
      math = math,
      substitutions = SubstitutionConfiguration.toMap(substitutions),
      xslt1version = xslt1version,
      xslt2version = xslt2version,
      epubEmbeddedFonts = Strings.toList(epubEmbeddedFonts),
      dataGeneratorClass = dataGeneratorClass,
      imagesDirectory = imagesDirectory
    )

    override def unparser: Unparser[DocumentConfiguration] = Unparser.concat[DocumentConfiguration](
      Configuration.nameAttribute(_.name),
      OutputConfiguration.seq(document => OutputConfiguration.fromSet(document.output)),
      ParameterConfiguration.seq(document => ParameterConfiguration.fromMap(document.parameters)),
      MathConfiguration.optional(_.math),
      SubstitutionConfiguration.seq(document => SubstitutionConfiguration.fromMap(document.substitutions)),
      Configuration.xslt1versionAttribute(_.xslt1version),
      Configuration.xslt1versionAttribute(_.xslt2version),
      Configuration.epubEmbeddedFontsAttribute(document => Strings.fromListOption(document.epubEmbeddedFonts)),
      dataGeneratorClassAttribute(_.dataGeneratorClass),
      imagesDirectoryAttribute(_.imagesDirectory)
    )
