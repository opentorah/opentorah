package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}

final case class TeiHeader(fileDesc: FileDesc, profileDesc: Option[ProfileDesc])

object TeiHeader {
  val elementName: String = "teiHeader"

  val parser: Parser[TeiHeader] = for {
    _ <- Xml.checkName(elementName)
    fileDesc <- FileDesc.parser
    profileDesc <- ProfileDesc.optionalParser
  } yield TeiHeader(
    fileDesc,
    profileDesc
  )
}
