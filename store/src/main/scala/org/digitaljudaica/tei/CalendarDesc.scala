package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}
import scala.xml.Elem

final case class CalendarDesc(xml: Elem)

object CalendarDesc {
  val elementName: String = "calendarDesc"

  val parser: Parser[CalendarDesc] = for {
    result <- Xml.next.element(elementName)
  } yield CalendarDesc(
    result
  )

  val optionalParser: Parser[Option[CalendarDesc]] =
    Xml.next.optional(elementName, parser)
}
