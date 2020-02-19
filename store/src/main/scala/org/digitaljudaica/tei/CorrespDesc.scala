package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}
import scala.xml.Elem

final case class CorrespDesc(xml: Elem)

object CorrespDesc {
  val elementName: String = "correspDesc"

  val parser: Parser[CorrespDesc] = for {
    result <- Xml.next.element(elementName)
  } yield CorrespDesc(
    result
  )

  val optionalParser: Parser[Option[CorrespDesc]] =
    Xml.next.optional(elementName, parser)
}
