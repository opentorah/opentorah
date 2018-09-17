package org.podval.calendar.metadata

import scala.xml.Elem

trait MetadataLoader extends ResourceLoader {
  final override protected def rootElementName: String = "metadata"

  final override protected def preparseMetadata(element: Elem): MetadataParser.MetadataPreparsed =
    MetadataParser.loadSubresource(getUrl, element, elementName)

  protected def elementName: String
}
