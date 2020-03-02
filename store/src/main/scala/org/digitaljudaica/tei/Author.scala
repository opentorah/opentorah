package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Author(xml: Seq[Node]) extends RawXml(xml)

object Author extends DescriptorRawXml[Author]("author", new Author(_))
