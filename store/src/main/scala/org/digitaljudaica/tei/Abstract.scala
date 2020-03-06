package org.digitaljudaica.tei

import org.digitaljudaica.xml.RawXml
import scala.xml.Node

final case class Abstract(xml: Seq[Node]) extends RawXml(xml)

object Abstract extends RawXml.Descriptor[Abstract]("abstract", new Abstract(_))
