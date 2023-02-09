package org.opentorah.docbook

import org.opentorah.math.MathConfiguration
import org.opentorah.util.Strings

final class Variant(
  val format: Format,
  val parameters: Map[String, String],
  val math: Option[MathConfiguration],
  val xslt1version: Option[String],
  val xslt2version: Option[String],
  val epubEmbeddedFonts: List[String],
  val configuration: Option[VariantConfiguration]
) extends HasName:
  override def name: String = format.name + configuration.fold("")(configuration => s"-${configuration.name}") // TODO unify with Distribution's

  def isDefault: Boolean = configuration.isEmpty
  
  def isEmpty: Boolean =
    parameters.isEmpty && (math.isEmpty || math.get.isEmpty) &&
    xslt1version.isEmpty && xslt2version.isEmpty && epubEmbeddedFonts.isEmpty && configuration.isEmpty

object Variant:
  // TODO split the format and variant names
  def forNames(output: Set[String], variants: Set[Variant]): Set[Variant] = for variantName <- output yield
    // TODO use HasName?
    variants.find(_.name == variantName).getOrElse {
      val (name: String, suffixOpt: Option[String]) = Strings.split(variantName, '-')
      if suffixOpt.isEmpty
      then Variant(
        format = Format.forName(name),
        parameters = Map.empty,
        math = None,
        xslt1version = None,
        xslt2version = None,
        epubEmbeddedFonts = List.empty,
        configuration = None
      )
      else throw IllegalArgumentException(s"Variant $variantName is not configured")
    }
