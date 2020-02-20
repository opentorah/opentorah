package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class LangUsage(xml: Elem)

object LangUsage extends DescriptorRaw[LangUsage](
  elementName = "langUsage",
  create = xml => new LangUsage(xml)
)
