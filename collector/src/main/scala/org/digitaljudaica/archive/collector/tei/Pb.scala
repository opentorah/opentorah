package org.digitaljudaica.archive.collector.tei

import org.digitaljudaica.xml.Ops._
import scala.xml.Elem

final case class Pb(
  n: String,
  facs: Option[String]
)

object Pb {

  // TODO rework with From/Parser
  def apply(xml: Elem): Pb = new Pb(
    n = xml.getAttribute("n"),
    facs = xml.attributeOption("facs")
  )
}
