package org.digitaljudaica.tei

import org.digitaljudaica.xml.{Descriptor, Xml}
import scala.xml.Node

final case class Creation(
  date: Date,
  xml: Seq[Node]
)

object Creation extends Descriptor[Creation](
  elementName = "creation",
  contentParser = for {
    date <- Date.required
    xml <- Xml.allNodes
  } yield new Creation(
    date,
    xml
  )
)
