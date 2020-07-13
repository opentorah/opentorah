package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, Parser}
import scala.xml.Node

final case class Date(
  when: String,
  calendar: Option[String],
  xml: Seq[Node]
)

object Date extends Element.WithToXml[Date]("date") {

  private val whenAttribute: Attribute[String] = Attribute("when")
  private val calendarAttribute: Attribute[String] = Attribute("calendar")

  override protected def contentType: ContentType = ContentType.Mixed

  override protected def parser: Parser[Date] = for {
    when <- whenAttribute.required
    calendar <- calendarAttribute.optional
    xml <- Element.allNodes
  } yield new Date(
    when,
    calendar,
    xml
  )

  override protected def attributes(value: Date): Seq[Attribute.Value[_]] = Seq(
    whenAttribute.withValue(value.when),
    calendarAttribute.withValue(value.calendar)
  )

  override protected def content(value: Date): Seq[Node] =
    value.xml
}
