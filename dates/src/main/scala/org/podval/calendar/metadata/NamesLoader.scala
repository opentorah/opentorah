package org.podval.calendar.metadata

import scala.xml.Elem

trait NamesLoader extends ResourceLoader {

  trait KeyBase extends Named.NamedBase { this: Key =>
    final override def names: Names = toMetadata(this)
  }

  override type Key <: KeyBase

  final override type BindableMetadata = Names

  final override type Metadata = Names

  final override protected def rootElementName: String = "names"

  final override protected def preparseMetadata(element: Elem): Names = Names.parse(element, None)

  final override protected def parseMetadata(key: Key, metadata: Names): Metadata = metadata
}
