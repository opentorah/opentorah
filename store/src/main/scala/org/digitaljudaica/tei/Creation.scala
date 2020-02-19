package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}
import scala.xml.Elem

final case class Creation(xml: Elem)

object Creation {
  val elementName: String = "creation"

  val parser: Parser[Creation] = for {
    result <- Xml.next.element(elementName)
  } yield Creation(
    result
  )

  val optionalParser: Parser[Option[Creation]] =
    Xml.next.optional(elementName, parser)
}
