package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}
import scala.xml.Elem

final case class LangUsage(xml: Elem)

object LangUsage {
  val elementName: String = "langUsage"

  val parser: Parser[LangUsage] = for {
    result <- Xml.next.element(elementName)
  } yield LangUsage(
    result
  )

  val optionalParser: Parser[Option[LangUsage]] =
    Xml.next.optional(elementName, parser)
}
