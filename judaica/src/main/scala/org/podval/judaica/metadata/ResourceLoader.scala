package org.podval.judaica.metadata

import scala.xml.Elem

trait ResourceLoader extends HasMetadata {
  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  final override protected lazy val toMetadata: Map[Key, Metadata] =
    bind(values, loadMetadataElements.map(preparseMetadata), parseMetadata)

  protected final def loadMetadataElements: Seq[Elem] = {
    val element = loadResource(resourceName)
    require(element.isDefined, s"No resource: $resourceName")
    val (attributes, elements) = XML.open(element.get, rootElementName)
    val typeOption = attributes.get("type")
    attributes.close()
    require(typeOption.nonEmpty, "Attribute 'type' is missing.")
    require(typeOption.contains(resourceName), s"Wrong metadata type: ${typeOption.get} instead of $resourceName")
    elements
  }

  protected def resourceName: String = Named.className(this)

  protected def rootElementName: String

  protected def preparseMetadata(element: Elem): BindableMetadata

  protected def parseMetadata(key: Key, metadata: BindableMetadata): Metadata
}
