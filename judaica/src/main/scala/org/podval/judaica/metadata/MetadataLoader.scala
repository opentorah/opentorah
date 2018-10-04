package org.podval.judaica.metadata

import scala.xml.Elem

trait MetadataLoader extends ResourceLoader with SubresourceLoader {
  final override protected def rootElementName: String = "metadata"

  final override type BindableMetadata = PreparsedMetadata

  final override protected def preparseMetadata(element: Elem): PreparsedMetadata =
    loadSubresource(element)
}
