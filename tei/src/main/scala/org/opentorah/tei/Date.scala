package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}

final case class Date(
  when: String,
  calendar: Option[String],
  xml: Seq[Xml.Node]
)

object Date extends Element[Date]("date") {

  private val whenAttribute: Attribute[String] = Attribute("when")
  private val calendarAttribute: Attribute[String] = Attribute("calendar")

  override def contentType: ContentType = ContentType.Mixed

  override def parser: Parser[Date] = for {
    when <- whenAttribute.required
    calendar <- calendarAttribute.optional
    xml <- Element.allNodes
  } yield new Date(
    when,
    calendar,
    xml
  )

  override val antiparser: Antiparser[Date] = Tei.concat(
    whenAttribute.toXml(_.when),
    calendarAttribute.toXmlOption(_.calendar),
    Antiparser.xml(_.xml)
  )
}
