package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser}
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

  override protected val antiparser: Antiparser[Date] = Antiparser(
    whenAttribute.toXml.compose(_.when),
    calendarAttribute.toXmlOption.compose(_.calendar),
    Antiparser.xml.compose(_.xml)
  )
}
