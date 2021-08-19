package org.opentorah.site

import org.opentorah.highlighter.Highlighter
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, ScalaXml, Unparser}

final class SiteHighlighter(
  val isEnabled: Boolean,
  val usePrism: Boolean
) {
  def head: Seq[ScalaXml.Element] = highlighter.head
  def body: Seq[ScalaXml.Element] = highlighter.body

  private def highlighter: Highlighter = Highlighter.get(usePrism)
}

object SiteHighlighter extends Element[SiteHighlighter]("highlighter") {

  val empty: SiteHighlighter = new SiteHighlighter(
    isEnabled = false,
    usePrism = false
  )

  val isEnabledAttribute: Attribute.OrDefault[Boolean] = new Attribute.BooleanAttribute("isEnabled").orDefault
  val usePrismAttribute: Attribute.OrDefault[Boolean] = new Attribute.BooleanAttribute("usePrism").orDefault

  override def contentParsable: Parsable[SiteHighlighter] = new Parsable[SiteHighlighter] {
    override def parser: Parser[SiteHighlighter] = for {
      isEnabled <- isEnabledAttribute()
      usePrism <- usePrismAttribute()
    } yield new SiteHighlighter(
      isEnabled,
      usePrism
    )

    override def unparser: Unparser[SiteHighlighter] = Unparser.concat[SiteHighlighter](
      isEnabledAttribute(_.isEnabled),
      usePrismAttribute(_.usePrism)
    )
  }
}