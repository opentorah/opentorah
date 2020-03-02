package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class RevisionDesc(xml: Seq[Node]) extends RawXml(xml)

object RevisionDesc extends DescriptorRawXml[RevisionDesc]("revisionDesc", new RevisionDesc(_))
