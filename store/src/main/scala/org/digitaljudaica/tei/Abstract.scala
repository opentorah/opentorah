package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}
import scala.xml.Elem

final case class Abstract(xml: Elem)

object Abstract {
  val elementName: String = "abstract"

  val parser: Parser[Abstract] = for {
    result <- Xml.next.element(elementName)
  } yield Abstract(
    result
  )

  val optionalParser: Parser[Option[Abstract]] =
    Xml.next.optional(elementName, parser)
}
