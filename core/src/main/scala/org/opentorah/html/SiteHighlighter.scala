package org.opentorah.html

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, ScalaXml, Unparser}

final class SiteHighlighter(
  val isEnabled: Boolean,
  val usePrism: Boolean
):
  def head: Seq[ScalaXml.Element] = highlighter.head
  def body: Seq[ScalaXml.Element] = highlighter.body

  private def highlighter: Highlighter = Highlighter.get(usePrism)

object SiteHighlighter extends Element[SiteHighlighter]("highlighter"):

  val empty: SiteHighlighter = SiteHighlighter(
    isEnabled = false,
    usePrism = false
  )

  override def contentParsable: Parsable[SiteHighlighter] = new Parsable[SiteHighlighter]:
    private val isEnabledAttribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("isEnabled").orDefault
    private val usePrismAttribute: Attribute.OrDefault[Boolean] = Attribute.BooleanAttribute("usePrism").orDefault
  
    override def parser: Parser[SiteHighlighter] = for
      isEnabled: Boolean <- isEnabledAttribute()
      usePrism: Boolean <- usePrismAttribute()
    yield SiteHighlighter(
      isEnabled,
      usePrism
    )

    override def unparser: Unparser[SiteHighlighter] = Unparser.concat[SiteHighlighter](
      isEnabledAttribute(_.isEnabled),
      usePrismAttribute(_.usePrism)
    )