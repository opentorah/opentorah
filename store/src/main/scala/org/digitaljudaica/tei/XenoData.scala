package org.digitaljudaica.tei

import org.digitaljudaica.xml.RawXml
import scala.xml.Node

final case class XenoData(xml: Seq[Node]) extends RawXml(xml)

object XenoData extends RawXml.Descriptor[XenoData]("xenoData", new XenoData(_))
