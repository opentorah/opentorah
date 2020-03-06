package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class Author(xml: Seq[Node]) extends RawXml(xml)

object Author extends RawXml.Descriptor[Author]("author", new Author(_))
