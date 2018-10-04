package org.podval.judaica.metadata

import scala.xml.Elem

final case class PreparsedMetadata(
  attributes: Attributes, // actual parser needs to call close()!
  names: Names,
  elements: Seq[Elem]
) extends Named.HasNames
