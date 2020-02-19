package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}
import scala.xml.Elem

// final case class FileDesc(publicationStmt: PublicationStmt, titleStmt: Option[TitleStmt])

final case class FileDesc(xml: Elem)

object FileDesc {
  val elementName: String = "fileDesc"

  val parser: Parser[FileDesc] = for {
    result <- Xml.next.element(elementName)
  } yield FileDesc(
    result
  )
}
