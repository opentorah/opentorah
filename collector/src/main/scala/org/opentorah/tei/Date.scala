package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, ElementTo, Nodes, Parsable, Parser, Unparser}

final class Date(
  val when: String,
  val calendar: Option[String],
  val xml: Nodes
)

object Date extends ElementTo[Date]("date"):
  val whenAttribute: Attribute.Required[String] = Attribute("when").required
  val calendarAttribute: Attribute.Optional[String] = Attribute("calendar").optional

  override def contentType: ContentType = ContentType.Mixed

  override def contentParsable: Parsable[Date] = new Parsable[Date]:
    override def parser: Parser[Date] = for
      when: String <- whenAttribute()
      calendar: Option[String] <- calendarAttribute()
      xml: Nodes <- Nodes.all()
    yield Date(
      when,
      calendar,
      xml
    )

    override val unparser: Unparser[Date] = Tei.concat(
      whenAttribute(_.when),
      calendarAttribute(_.calendar),
      Nodes.all(_.xml)
    )
