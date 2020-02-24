package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Author(xml: Elem)

object Author extends DescriptorRaw[Author]("author", new Author(_))
