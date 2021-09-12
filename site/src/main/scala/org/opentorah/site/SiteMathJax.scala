package org.opentorah.site

import org.opentorah.mathjax.{Delimiters, MathJax, MathJaxConfiguration}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, ScalaXml, Unparser}

final class SiteMathJax(
  val isEnabled: Boolean,
  val useV3: Boolean
):
  // TODO
  //  val mathJaxConfiguration: MathJaxConfiguration = MathJaxConfiguration(
  //    font = mathJaxFont.get,
  //    extensions = mathJaxExtensions.get.asScala.toList,
  //    texDelimiters = Delimiters(texDelimiter.get),
  //    texInlineDelimiters = Delimiters(texInlineDelimiter.get),
  //    asciiMathDelimiters = Delimiters(asciiMathDelimiter.get),
  //    processEscapes = processMathJaxEscapes.get
  //  )
  def configuration: MathJaxConfiguration = MathJaxConfiguration(
    extensions = List.empty,
    texDelimiters = Delimiters("$$"),
    texInlineDelimiters = Delimiters("$")
  )

  def body: ScalaXml.Nodes = mathJax.body(ScalaXml.mkText(mathJax.htmlConfigurationString(configuration)))

  private def mathJax: MathJax = MathJax.get(useV3)

object SiteMathJax extends Element[SiteMathJax]("mathJax"):

  val empty: SiteMathJax = SiteMathJax(
    isEnabled = false,
    useV3 = true
  )

  val isEnabledAttribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("isEnabled").orDefault
  val useV3Attribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("useV3").orDefault

  override def contentParsable: Parsable[SiteMathJax] = new Parsable[SiteMathJax]:
    override def parser: Parser[SiteMathJax] = for
      isEnabled: Boolean <- isEnabledAttribute()
      useV3: Boolean <- useV3Attribute()
    yield SiteMathJax(
      isEnabled,
      useV3
    )

    override def unparser: Unparser[SiteMathJax] = Unparser.concat[SiteMathJax](
      isEnabledAttribute(_.isEnabled),
      useV3Attribute(_.useV3)
    )

