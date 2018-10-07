package org.podval.judaica.metadata

import java.io.FileNotFoundException

import scala.xml.{Elem, Utility}

final case class Metadata(
  attributes: Attributes, // actual parser needs to call close()!
  names: Names,
  elements: Seq[Elem]
) extends HasNames

object Metadata {
  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  def loadNames[K <: Named](
    values: Seq[K],
    obj: AnyRef,
    resourceName: String
  ): Map[K, Names] = bind(
    values,
    loadMetadataElements(obj, resourceName, rootElementName = "names")
      .map(element => Names.parse(element, None))
  )

  def loadMetadata[K <: Named, M <: HasName](
    values: Seq[K],
    obj: AnyRef,
    resourceName: String,
    rootElementName: String,
    elementName: String
  ): Map[K, Metadata] = bind(
    values,
    loadMetadataElements(obj, resourceName, rootElementName = "metadata")
      .map(element => loadSubresource(obj, element, elementName))
  )

  private def apply(attributes: Attributes, elements: Seq[Elem]): Metadata = {
    val (names: Names, tail: Seq[Elem]) = Names.parse(attributes, elements)
    Metadata(attributes, names, tail)
  }

  private def loadMetadataElements(obj: AnyRef, resourceName: String, rootElementName: String): Seq[Elem] = {
    val element = loadResource(obj, resourceName)
    val (attributes, elements) = XML.open(element, rootElementName)
    val type_ = attributes.doGet("type")
    attributes.close()
    require(type_ == resourceName, s"Wrong metadata type: $type_ instead of $resourceName")
    elements
  }

  private def loadResource(obj: AnyRef, resourceName: String): Elem = {
    val url = Option(obj.getClass.getResource(resourceName + ".xml"))
    val result = url.flatMap { url =>
      try {
        val result = xml.XML.load(url.openStream())
        Some(Utility.trimProper(result).asInstanceOf[Elem])
      } catch {
        case _: FileNotFoundException => None
      }
    }

    require(result.isDefined, s"No resource: $resourceName")
    result.get
  }

  // TODO make private?
  final def loadSubresource(obj: AnyRef, element: Elem, elementName: String): Metadata = {
    val (attributes, elements) = XML.open(element, elementName)
    attributes.get("resource").fold(Metadata(attributes, elements)) { subresourceName: String =>
      attributes.close()
      val subresource: Elem = loadResource(obj, subresourceName)
      val (newAttributes, newElements) = XML.open(subresource, elementName)
      Metadata(newAttributes, newElements)
    }
  }

  // TODO make private?
  // This is used to bind both Metadata and names, so - HasName...
  final def bind[K <: Named, M <: HasName](
    keys: Seq[K],
    metadatas: Seq[M]
  ): Map[K, M] = {
    require(keys.length == metadatas.length)

    // TODO disjoint

    // TODO relax the "same order" requirement.
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.hasName(key.name), s"Metadata entry $metadata doesn't have the name ${key.name}")
      key -> metadata
    }.toMap
  }
}
