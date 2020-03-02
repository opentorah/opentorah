package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class CorrespDesc(xml: Seq[Node]) extends RawXml(xml)

object CorrespDesc extends DescriptorRawXml[CorrespDesc]("correspDesc", new CorrespDesc(_))
