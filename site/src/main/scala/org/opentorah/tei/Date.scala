package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class Date(
  val when: String,
  val calendar: Option[String],
  val xml: Element.Nodes
)

object Date extends Element[Date]("date"):
  val whenAttribute: Attribute.Required[String] = Attribute("when").required
  val calendarAttribute: Attribute.Optional[String] = Attribute("calendar").optional

  override def contentType: Element.ContentType = Element.ContentType.Mixed

  override def contentParsable: Parsable[Date] = new Parsable[Date]:
    override def parser: Parser[Date] = for
      when: String <- whenAttribute()
      calendar: Option[String] <- calendarAttribute()
      xml: Element.Nodes <- Element.nodes()
    yield Date(
      when,
      calendar,
      xml
    )

    override val unparser: Unparser[Date] = Tei.concat(
      whenAttribute(_.when),
      calendarAttribute(_.calendar),
      Element.nodes(_.xml)
    )
