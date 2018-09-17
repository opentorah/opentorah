package org.podval.calendar.metadata

import java.net.URL

import scala.xml.Elem

trait ResourceLoader extends WithMetadata {
  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  final override protected lazy val toMetadata: Map[Key, Metadata] =
    bind(values, loadMetadataElements.map(preparseMetadata), parseMetadata)

  protected final def loadMetadataElements: Seq[Elem] = {
    val url = getUrl
    val element = MetadataParser.loadResource(url)
    require(element.isDefined, s"No resource: $url")
    val (attributes, elements) = XML.open(element.get, rootElementName)
    val typeOption = attributes.get("type")
    attributes.close()
    require(typeOption.nonEmpty, "Attribute 'type' is missing.")
    require(typeOption.contains(resourceName), s"Wrong metadata type: ${typeOption.get} instead of $resourceName")
    elements
  }

  protected final def getUrl: URL = {
    val result = getClass.getResource(resourceName + ".xml")
    require(result != null, s"No such resource: $this:$resourceName")
    result
  }

  protected def resourceName: String = Named.className(this)

  protected def rootElementName: String

  // TODO fold into the WithMetadata...

  protected def preparseMetadata(element: Elem): BindableMetadata

  protected def parseMetadata(key: Key, metadata: BindableMetadata): Metadata
}
