package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.Descriptor

final case class TeiHeader(fileDesc: FileDesc, profileDesc: Option[ProfileDesc])

object TeiHeader extends Descriptor[TeiHeader](
  elementName = "teiHeader",
  contentParser = for {
    fileDesc <- FileDesc.required
    profileDesc <- ProfileDesc.optional
  } yield new TeiHeader(
    fileDesc,
    profileDesc
  )
)
