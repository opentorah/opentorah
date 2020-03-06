package org.digitaljudaica.tei

import org.digitaljudaica.xml.RawXml
import scala.xml.Node

final case class Author(xml: Seq[Node]) extends RawXml(xml)

object Author extends RawXml.Descriptor[Author]("author", new Author(_))
