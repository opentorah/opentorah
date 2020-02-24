package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class LangUsage(xml: Elem)

object LangUsage extends DescriptorRaw[LangUsage]("langUsage", new LangUsage(_))
