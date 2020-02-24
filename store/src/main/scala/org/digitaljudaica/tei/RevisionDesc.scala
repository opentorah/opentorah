package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class RevisionDesc(xml: Elem)

object RevisionDesc extends DescriptorRaw[RevisionDesc]("revisionDesc", new RevisionDesc(_))
