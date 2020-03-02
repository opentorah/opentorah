package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class EncodingDesc(xml: Seq[Node]) extends RawXml(xml)

object EncodingDesc extends DescriptorRawXml[EncodingDesc]("encodingDesc", new EncodingDesc(_))
