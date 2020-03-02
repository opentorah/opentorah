package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class RevisionDesc(xml: Elem) extends RawXml(xml)

object RevisionDesc extends DescriptorRawXml[RevisionDesc]("revisionDesc", new RevisionDesc(_))
