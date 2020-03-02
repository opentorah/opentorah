package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class SourceDesc(xml: Seq[Node]) extends RawXml(xml)

object SourceDesc extends DescriptorRawXml[SourceDesc]("sourceDesc", new SourceDesc(_))
