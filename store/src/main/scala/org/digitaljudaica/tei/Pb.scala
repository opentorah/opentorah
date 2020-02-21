package org.digitaljudaica.tei

import org.digitaljudaica.xml.{ContentType, Descriptor, Xml}

final case class Pb(
  n: String,
  id: Option[String],
  facs: Option[String]
)

object Pb extends Descriptor[Pb](
  elementName = "pb",
  contentType = ContentType.Empty,
  contentParser = for {
    n <- Xml.attribute.required("n")
    id <- Xml.attribute.optional.id
    facs <- Xml.attribute.optional("facs")
  } yield new Pb(
    n,
    id,
    facs
  )
)
