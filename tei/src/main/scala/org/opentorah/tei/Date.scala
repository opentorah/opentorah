package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, ContentType, Element, Parsable, Parser, Xml}

final case class Date(
  when: String,
  calendar: Option[String],
  xml: Xml.Nodes
)

object Date extends Element[Date]("date") {

  private val whenAttribute: Attribute.Required[String] = Attribute("when").required
  private val calendarAttribute: Attribute.Optional[String] = Attribute("calendar").optional

  override def contentType: ContentType = ContentType.Mixed

  override def contentParsable: Parsable[Date] = new Parsable[Date] {
    override def parser: Parser[Date] = for {
      when <- whenAttribute()
      calendar <- calendarAttribute()
      xml <- Element.nodes()
    } yield new Date(
      when,
      calendar,
      xml
    )

    override val unparser: Unparser[Date] = Tei.concat(
      whenAttribute(_.when),
      calendarAttribute(_.calendar),
      Element.nodes(_.xml)
    )
  }
}
